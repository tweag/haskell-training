module Domain.Question where

import Domain.Id
import Domain.Questionnaire

-- text
import Data.Text

--uuid
import Data.UUID

data AnswerType
  = Paragraph
  | Number

newtype QuestionnaireId = QuestionnaireId UUID

data Question = Question
  { title :: Text
  , answerType :: AnswerType
  , questionnaireId :: Id Questionnaire
  }
