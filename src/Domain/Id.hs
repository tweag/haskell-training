{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Domain.Id where

-- aeson
import Data.Aeson.Types

-- base
import GHC.Generics

-- openapi3
import Data.OpenApi

-- uuid
import Data.UUID

newtype Id a = Id UUID
  deriving (Generic, FromJSON, ToJSON, ToSchema, ToParamSchema)

data Identified a = Identified
  { id     :: Id a
  , entity :: a
  }
  deriving (Generic, FromJSON, ToJSON)

instance ToSchema a => ToSchema (Identified a)
