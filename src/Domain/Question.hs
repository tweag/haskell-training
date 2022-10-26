module Domain.Question where

import Domain.Id
import Domain.Questionnaire

-- text
import Data.Text

data Question = Question
  { title           :: Text
  , answerType      :: AnswerType
  , questionnaireId :: Id Questionnaire
  }

data AnswerType
  = Paragraph
  | Number
