module Handler.Queries where

import Import
import Model()

getQuestionQueryR :: Query -> QuestionId -> Handler RepJson
getQuestionQueryR _ _ = undefined

getAnswerQueryR :: Query -> AnswerId -> Handler RepJson
getAnswerQueryR QueryVotes answerId = do
  maid <- maybeAuth
  answer <- runDB $ get404 answerId
  case maid of
    (Just (Entity uid _)) -> do
      mact <- runDB $ getBy $ UniqueUserActivity uid
      case mact of
        Just (Entity _ (Activity _ as _)) ->
          if answerId `elem` as then
            jsonToRepJson $ object ["votes" .= (answerUpvotes answer),
                                    "upvoted" .= True]
          else
            jsonToRepJson $ object ["votes" .= (answerUpvotes answer),
                                    "upvoted" .= False]
        _ -> jsonToRepJson $ object ["votes" .= (answerUpvotes answer)]
    _ -> jsonToRepJson $ object ["votes" .= (answerUpvotes answer)]