module Handler.Question where

import Import
import Yesod.Markdown
import Model()
import Handler.Forms
import qualified Data.Text as T
import Data.Maybe (isJust, fromJust)
import Data.Time (getCurrentTime)

type QuestionPair = (QuestionId, Question)
type AskerPair = (UserId, User)
type QuestionListResult = [(QuestionPair, [AskerPair])]

nullWidget :: Widget
nullWidget = do
  toWidget [hamlet| <span>|]

timeWidget :: UTCTime -> Widget
timeWidget asked = do
  toWidget [hamlet|<div .time>#{show asked}|]

fullUserWidget :: DefaultUser -> UserId -> Widget -> Widget
fullUserWidget user userId widget = do
  toWidget [whamlet|
            <div .user-box>
                <img .span-3 src=#{getGravatar $ defaultUserIdent user} alt=#{defaultUserName user}>
                <div .span-11 .last>
                    ^{widget}
                    <a href=@{UserViewR userId}>
                        #{defaultUserName user}
                    <p>
                        #{defaultUserReputation user}
                   |]

getQuestionListR :: Handler RepHtml
getQuestionListR = do
  (pageM, sortM) <- runInputGet $ (,)
           <$> iopt intField "page"
           <*> iopt textField "sort"
  let page = maybe 0 id pageM
      sort = "sort"
  questions <- runDB $
               selectList [QuestionTitle !=. ""] [LimitTo 10,
                                                  Desc QuestionAsked] >>=
               mapM (\qe@(Entity _ q) -> do
                        asker <- get $ questionAsker q
                        return (qe, asker))
  defaultLayout $ do
    setTitle "Questions"
    $(widgetFile "questionList")

getQuestionViewR :: QuestionId -> Handler RepHtml
getQuestionViewR questionId = do
  mu <- maybeAuth
  (question@(Question title body tags _ _ _ _ _ _), answers, askerU) <- runDB $ do
    question@(Question _ _ _ aIds askerI _ _ _ _) <- get404 questionId
    answers <- liftM (zipWith (,) aIds) $ mapM get aIds
    asker <- getUser askerI
    let answersJust = map (\(uId, ma) -> (uId, fromJust ma)) $ filter (isJust . snd) answers
        answerers' = map (\(_, (Answer _ a _ _ _ _)) -> a) answersJust
    answerers <- mapM getUser answerers'
    let answers' = zipWith (\(a,b) c -> (a, b, c)) answersJust answerers
    return (question, answers', asker)
  ((_, formWidget), enctype) <- generateFormPost answerForm
  let qbody = markdownToHtml $ Markdown $ T.unpack body
  defaultLayout $ do
    case tags of
      [] -> setTitle $ toHtml title
      (x:_) -> setTitle $ toHtml $ T.concat [x, T.pack " - ", title]

    $(widgetFile "questionView")

postQuestionViewR :: QuestionId -> Handler RepHtml
postQuestionViewR questionId = do
  Entity uid _ <- requireAuth
  ((result, _), _) <- runFormPost answerForm
  case result of
    FormSuccess (Markdown m) -> do
      time <- liftIO getCurrentTime
      runDB $ do
        (Question _ _ _ answers _ _ _ _ _) <- get404 questionId
        aid <- insert $ Answer (T.pack m) uid 0 0 time Nothing
        update questionId [QuestionAnswers =. aid:answers]
      redirect (QuestionViewR questionId)
    _ ->
      redirect (QuestionViewR questionId)