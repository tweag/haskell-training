{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Domain.Question where

import Domain.Id
import Domain.Questionnaire

-- aeson
import Data.Aeson.Types

-- base
import GHC.Generics

-- openapi3
import Data.OpenApi

-- text
import Data.Text

data AnswerType
  = Paragraph
  | Number
  deriving (Generic, FromJSON, ToJSON, ToSchema)

data Question = Question
  { title :: Text
  , answerType :: AnswerType
  , questionnaireId :: Id Questionnaire
  }
  deriving (Generic, FromJSON, ToJSON, ToSchema)
