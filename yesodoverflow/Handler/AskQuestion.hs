module Handler.AskQuestion where

import Import
import Yesod.Markdown
import Model()
import Data.Text (splitOn, singleton, pack)
import Handler.Forms
import Data.Time (getCurrentTime)

getAskR :: Handler RepHtml
getAskR = do
  Entity uid u <- requireAuth
  ((_, formWidget), enctype) <- generateFormPost askForm
  defaultLayout $ do
    setTitle "Ask Question"
    $(widgetFile "ask")

postAskFinalizeR :: Handler RepHtml
postAskFinalizeR = do
  Entity uid u <- requireAuth
  ((result, formWidget), enctype) <- runFormPost askForm
  case result of
    FormSuccess (PartialQuestion a (Markdown b) c) -> do
      let tags = maybe [] (splitOn (singleton ',')) c
      time <- liftIO getCurrentTime
      toid <- runDB $ insert $ Question a (pack b) tags [] uid 0 0 time Nothing
      redirect $ QuestionViewR toid
    _ -> defaultLayout $ do
      setTitle "Ask Question"
      $(widgetFile "ask")