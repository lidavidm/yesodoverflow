module Import
    ( module Prelude
    , module Yesod
    , module Foundation
    , (<>)
    , Text
    , module Data.Monoid
    , module Control.Applicative
    , UTCTime
    , module T
    , liftM
    , getUser
    , getGravatar
    , DefaultUser (..)
    ) where

import Prelude hiding (writeFile, readFile)
import Yesod   hiding (Route(..))
import Yesod.Auth
import Foundation
import Data.Monoid (Monoid (mappend, mempty, mconcat))
import Control.Applicative ((<$>), (<*>), pure)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.ByteString.Lazy.Char8 as B
import Control.Monad
import Data.Digest.Pure.MD5 (md5)
import Data.Maybe (maybe)
import Data.Time (UTCTime)

infixr 5 <>
(<>) :: Monoid m => m -> m -> m
(<>) = mappend

data DefaultUser = DefaultUser {defaultUserIdent :: Text,
                                defaultUserName :: Text,
                                defaultUserReputation :: Int,
                                defaultUserActivity :: Activity}
                   deriving Show

getUser userId = do
  u <- get userId
  case u of
    Just (User ident name rep _) -> do
      activity <- getBy $ UniqueUserActivity userId
      let activity' = maybe (Activity [] [] undefined) entityVal activity
      return $ DefaultUser ident name rep activity'
    Nothing -> return $ DefaultUser
               "<deleted user>" "<deleted user>" 0 (Activity [] [] undefined)

getGravatar :: Text -> [Char]
getGravatar email = concat [
  "http://www.gravatar.com/avatar/",
  show $ md5 $ B.pack $ T.unpack $ T.strip $ T.toLower $ email,
  "?d=identicon"
  ]