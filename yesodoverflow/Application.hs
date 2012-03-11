{-# OPTIONS_GHC -fno-warn-orphans #-}
module Application
    ( getApplication
    , getApplicationDev
    ) where

import Import
import Settings
import Settings.StaticFiles (staticSite)
import Yesod.Auth
import Yesod.Default.Config
import Yesod.Default.Main
import Yesod.Default.Handlers
#if DEVELOPMENT
import Yesod.Logger (Logger, logBS)
import Network.Wai.Middleware.RequestLogger (logCallbackDev)
#else
import Yesod.Logger (Logger, logBS, toProduction)
import Network.Wai.Middleware.RequestLogger (logCallback)
import qualified Web.Heroku
#endif
import qualified Database.Persist.Store
import Database.Persist.GenericSql (runMigration)
import Network.HTTP.Conduit (newManager, def)

-- Import all relevant handler modules here.
import Handler.Root
import Handler.Question
import Handler.AskQuestion
import Handler.User
import Handler.Actions

-- This line actually creates our YesodSite instance. It is the second half
-- of the call to mkYesodData which occurs in Foundation.hs. Please see
-- the comments there for more details.
mkYesodDispatch "Overflow" resourcesOverflow

-- This function allocates resources (such as a database connection pool),
-- performs initialization and creates a WAI application. This is also the
-- place to put your migrate statements to have automatic database
-- migrations handled by Yesod.
getApplication :: AppConfig DefaultEnv Extra -> Logger -> IO Application
getApplication conf logger = do
    manager <- newManager def
    s <- staticSite
    dbconf <- withYamlEnvironment "config/sqlite.yml" (appEnv conf)
              Database.Persist.Store.loadConfig >>=
              Database.Persist.Store.applyEnv
    p <- Database.Persist.Store.createPoolConfig (dbconf :: Settings.PersistConfig)
    Database.Persist.Store.runPool dbconf (runMigration migrateAll) p
    let foundation = Overflow conf setLogger s p manager dbconf
    app <- toWaiAppPlain foundation
    return $ logWare app
  where
#ifdef DEVELOPMENT
    logWare = logCallbackDev (logBS setLogger)
    setLogger = logger
#else
    setLogger = toProduction logger -- by default the logger is set for development
    logWare = logCallback (logBS setLogger)
#endif

-- for yesod devel
getApplicationDev :: IO (Int, Application)
getApplicationDev =
    defaultDevelApp loader getApplication
  where
    loader = loadConfig (configSettings Development)
        { csParseExtra = parseExtra
        }

canonicalizeKey :: (Text, val) -> (Text, val)
canonicalizeKey ("dbname", val) = ("database", val)
canonicalizeKey pair = pair

toMapping :: [(key, val)] -> DO.Object key val
toMapping = DO.Mapping . map (\(key, val) -> (key, DO.Scalar val))

combineMappings :: DO.Object key val -> DO.Object key val -> DO.Object key val
combineMappings (DO.Mapping m1) (DO.Mapping m2) = DO.Mapping $ m1 ++ m2
combineMappings _ _ = error "Data.Object is not a Mapping."

loadHerokuConfig :: DO.TextObject -> IO Settings.PersistConfig
loadHerokuConfig ymlenv = do
  #if DEVELOPMENT
      let urlMap = DO.Mapping []
  #else
      urlMap <- Web.Heroku.dbConnParams >>= return . toMapping . map canonicalizeKey
  #endif
      either error return $ Database.Persist.Base.loadConfig (combineMappings urlMap ymlenv)


withYesodHeroku :: AppConfig DefaultEnv () -> Logger -> (Application -> IO ()) -> IO ()
withYesodHeroku conf logger f = do
s <- staticSite
dbconf <- withYamlEnvironment "config/postgresql.yml" (appEnv conf) loadHerokuConfig
Database.Persist.Base.withPool (dbconf :: Settings.PersistConfig) $ \p -> do
  Database.Persist.Base.runPool dbconf (runMigration migrateAll) p
  let h = YesodHeroku conf logger s p
  defaultRunner (f . logWare) h
  where
    #ifdef DEVELOPMENT
       logWare = logStdoutDev
    #else
       logWare = logStdout
    #endif