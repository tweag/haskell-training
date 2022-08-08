{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}

module Domain.Question where

import Domain.Id
import Domain.Questionnaire

-- aeson
import Data.Aeson hiding (Number)

-- text
import Data.Text

--uuid
import Data.UUID

data AnswerType
  = Paragraph
  | Number

instance ToJSON AnswerType where
  toJSON :: AnswerType -> Value
  toJSON Paragraph = "Paragraph"
  toJSON Number    = "Number"

newtype QuestionnaireId = QuestionnaireId UUID

data Question = Question
  { title :: Text
  , answerType :: AnswerType
  , questionnaireId :: Id Questionnaire
  }

instance ToJSON Question where
  toJSON :: Question -> Value
  toJSON (Question title answerType questionnaireId) = object
    [ "title"           .= title
    , "answerType"      .= answerType
    , "questionnaireId" .= questionnaireId
    ]
