module Domain.Servant.Question where

import Domain.Servant.Id
import Domain.Servant.Questionnaire

-- text
import Data.Text

data AnswerType
  = Paragraph
  | Number

data Question = Question
  { title           :: Text
  , answerType      :: AnswerType
  , questionnaireId :: Id Questionnaire
  }
