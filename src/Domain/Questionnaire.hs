{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

module Domain.Questionnaire where

-- aeson
import Data.Aeson.Types

-- base
import GHC.Generics

-- openapi3
import Data.OpenApi

-- text
import Data.Text

newtype Questionnaire = Questionnaire
  { title :: Text
  }
  deriving stock Generic
  deriving anyclass (FromJSON, ToJSON, ToSchema)
