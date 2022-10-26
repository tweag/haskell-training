{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Domain.Answer where

import Domain.Id
import Domain.Question

-- aeson
import Data.Aeson.Types

-- base
import GHC.Generics

-- openapi3
import Data.OpenApi

-- text
import Data.Text

data Submission

data Answer = Answer
  { content      :: Content
  , questionId   :: Id Question
  , submissionId :: Id Submission
  }
  deriving (Generic, FromJSON, ToJSON, ToSchema)

data AnswerData = AnswerData
  { contentData    :: Content
  , questionIdData :: Id Question
  }
  deriving (Generic, FromJSON, ToJSON, ToSchema)

data Content
  = Paragraph Text
  | Number Int
  deriving (Generic, FromJSON, ToJSON, ToSchema)
