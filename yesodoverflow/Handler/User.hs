module Handler.User where

import Import
import Model()

getUserViewR :: UserId -> Handler RepHtml
getUserViewR userId = do
  mu <- maybeAuth
  (User ident name reputation  _) <- runDB $ get404 userId
  defaultLayout $ do
    $(widgetFile "userView")