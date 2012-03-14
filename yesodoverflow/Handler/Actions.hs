module Handler.Actions where

import Import
import Model()

data ActionError = NotLoggedIn | InvalidUser | AlreadyUpvoted

actionErrorMessage :: ActionError -> Text
actionErrorMessage NotLoggedIn = "Not logged in."
actionErrorMessage InvalidUser = "Invalid user."
actionErrorMessage AlreadyUpvoted = "Already upvoted."

actionFail :: ActionError -> Value
actionFail err = object [
  "succeeded" .= False,
  "errorMessage" .= actionErrorMessage err
  ]

runAction action = do
  maid <- maybeAuth
  case maid of
    (Just (Entity uid _)) -> do
      activity <- runDB $ getBy $ UniqueUserActivity uid
      case activity of
        Just (Entity aid entity') -> case action aid entity' of
            Right (val, dbAction) -> do
              _ <- runDB dbAction
              jsonToRepJson val
            Left val -> jsonToRepJson val
        Nothing -> jsonToRepJson $ actionFail InvalidUser
    Nothing -> jsonToRepJson $ actionFail NotLoggedIn

putQuestionActionR :: Action -> QuestionId -> Handler RepJson
putQuestionActionR Upvote questionId = runAction $ \aid (Activity qs _  _) -> do
  if questionId `notElem` qs then do
    Right $ (object [("succeeded", True)],
             update questionId [QuestionUpvotes +=. 1] >>
             update aid [ActivityUpvotedQuestions =. questionId:qs])
    else do
    Left $ actionFail AlreadyUpvoted

putQuestionActionR Downvote _ = undefined

putAnswerActionR :: Action -> AnswerId -> Handler RepJson
putAnswerActionR Upvote answerId = runAction $ \aid (Activity _ as _) -> do
  if answerId `notElem` as then do
    Right $ (object [("succeeded", True)],
             update answerId [AnswerUpvotes +=. 1] >>
             update aid [ActivityUpvotedAnswers =. answerId:as])
    else do
    Left $ actionFail AlreadyUpvoted

putAnswerActionR Downvote _ = undefined