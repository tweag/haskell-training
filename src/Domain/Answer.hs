{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

module Domain.Answer where

import Domain.Id
import Domain.Question hiding (Paragraph, Number)

-- aeson
import Data.Aeson

-- base
import GHC.Generics

-- openapi3
import Data.OpenApi

-- text
import Data.Text

data Answer = Answer
  { content    :: Content
  , questionId :: Id Question
  }
  deriving (Generic, FromJSON, ToJSON, ToSchema)

data Content
  = Paragraph Text
  | Number Int
  deriving (Generic, FromJSON, ToJSON, ToSchema)

data AnswerSet
