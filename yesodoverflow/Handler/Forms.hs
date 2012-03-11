module Handler.Forms where

import Import
import Yesod.Markdown

data PartialQuestion = PartialQuestion Text Markdown (Maybe Text)
                     deriving Show

data QuestionListQuery = QuestionListQuery (Maybe Int) (Maybe Text)
                       deriving Show

askForm :: Form PartialQuestion
askForm = renderDivs $ PartialQuestion
          <$> areq textField "Title" Nothing
          <*> areq markdownField "Body" Nothing
          <*> aopt textField "Tags" Nothing

answerForm :: Form Markdown
answerForm = renderDivs $ areq markdownField "Response" Nothing