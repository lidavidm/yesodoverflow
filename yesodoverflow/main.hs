import Prelude              (IO)
import System.Environment
import Yesod.Default.Config (fromArgs)
import Yesod.Default.Main   (defaultMain)
import Settings             (parseExtra)
import Application          (getApplication)

main :: IO ()
main = do
  durl <- getEnv "DATABASE_URL"
  defaultMain (fromArgsExtra loadExtra) withYesodHeroku
