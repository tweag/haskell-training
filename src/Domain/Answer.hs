{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}

module Domain.Answer where

import Domain.Id
import Domain.Question

-- aeson
import Data.Aeson

-- base
import GHC.Generics

-- openapi3
import Data.OpenApi

-- rel8
import Rel8

-- text
import Data.Text

data Content
  = Paragraph Text
  | Number Int
  deriving stock (Generic, Read, Show)
  deriving anyclass (FromJSON, ToJSON, ToSchema)
  deriving DBType via ReadShow Content

data AnswerData = AnswerData
  { contentData    :: Content
  , questionIdData :: Id Question
  }
  deriving (Generic, FromJSON, ToJSON, ToSchema)

data AnswerSet

data Answer = Answer
  { content    :: Content
  , questionId :: Id Question
  , setId      :: Id AnswerSet
  }
  deriving (Generic, FromJSON, ToJSON, ToSchema)
