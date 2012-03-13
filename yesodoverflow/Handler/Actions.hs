module Handler.Actions where

import Import
import Model()
import Data.Maybe (isNothing)

data ActionError = NotLoggedIn | InvalidUser | AlreadyUpvoted

actionErrorMessage :: ActionError -> Text
actionErrorMessage NotLoggedIn = "Not logged in."
actionErrorMessage InvalidUser = "Invalid user."
actionErrorMessage AlreadyUpvoted = "Already upvoted."

actionFail err = jsonToRepJson $ object [
      "succeeded" .= False,
      "errorMessage" .= actionErrorMessage err
      ]

putQuestionActionR :: Action -> QuestionId -> Handler RepJson
putQuestionActionR Upvote questionId = do
  maid <- maybeAuth
  case maid of
    (Just (Entity uid _)) -> do
      activity <- runDB $ getBy $ UniqueUserActivity uid
      case activity of
        Just (Entity aid (Activity upvoted _)) -> do
          if questionId `notElem` upvoted then do
            runDB $ do
              update questionId [QuestionUpvotes +=. 1]
              update aid [ActivityUpvoted =. questionId:upvoted]
            jsonToRepJson $ object [("succeeded", True)]
            else
            actionFail AlreadyUpvoted
        Nothing -> actionFail InvalidUser
    Nothing -> actionFail NotLoggedIn

putQuestionActionR Downvote _ = undefined