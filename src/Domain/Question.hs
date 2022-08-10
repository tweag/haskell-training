{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

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

--uuid
import Data.UUID

data AnswerType
  = Paragraph
  | Number
  deriving stock Generic
  deriving anyclass (FromJSON, ToJSON, ToSchema)

newtype QuestionnaireId = QuestionnaireId UUID

data Question = Question
  { title :: Text
  , answerType :: AnswerType
  , questionnaireId :: Id Questionnaire
  }
  deriving stock Generic
  deriving anyclass (FromJSON, ToJSON, ToSchema)
