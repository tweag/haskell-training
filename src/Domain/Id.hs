{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Domain.Id where

-- aeson
import Data.Aeson.Types

-- base
import GHC.Generics

-- uuid
import Data.UUID

newtype Id a = Id UUID
  deriving (Generic, FromJSON, ToJSON)

data Identified a = Identified
  { id     :: Id a
  , entity :: a
  }
  deriving (Generic, FromJSON, ToJSON)
