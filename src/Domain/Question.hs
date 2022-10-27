
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}

module Domain.Question where

import Domain.Id
import Domain.Questionnaire

-- aeson
import Data.Aeson.Types

-- base
import GHC.Generics

-- openapi3
import Data.OpenApi

-- rel8
import Rel8

-- text
import Data.Text

data Question = Question
  { title           :: Text
  , answerType      :: AnswerType
  , questionnaireId :: Id Questionnaire
  }
  deriving (Generic, FromJSON, ToJSON, ToSchema)

data AnswerType
  = Paragraph
  | Number
  deriving stock (Read, Show, Generic)
  deriving DBType via ReadShow AnswerType
  deriving anyclass (FromJSON, ToJSON, ToSchema)
