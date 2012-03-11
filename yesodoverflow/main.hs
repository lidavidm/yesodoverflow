import Prelude              (IO)
import System.Environment
import Yesod.Default.Config (fromArgs)
import Yesod.Default.Main   (defaultMain)
import Settings             (parseExtra)
import Application          (getApplication)

main :: IO ()
main = defaultMain (fromArgs parseExtra) getApplication
