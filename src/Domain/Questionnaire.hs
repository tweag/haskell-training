{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Domain.Questionnaire where

-- aeson
import Data.Aeson.Types

-- base
import GHC.Generics

-- openapi3
import Data.OpenApi

-- text
import Data.Text

data Questionnaire = Questionnaire
  { title :: Text
  }
  deriving (Generic, FromJSON, ToJSON, ToSchema)
