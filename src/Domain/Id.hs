{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Domain.Id where

-- aeson
import Data.Aeson.Types

-- base
import GHC.Generics

-- openapi3
import Data.OpenApi

-- rel8
import Rel8

--uuid
import Data.UUID
import Data.UUID.V4

newtype Id a = Id UUID
  deriving newtype (Show, FromJSON, ToJSON, ToSchema, ToParamSchema, DBType, DBEq)

generate :: IO (Id a)
generate = Id <$> nextRandom

data Identified a = Identified
  { id     :: Id a
  , entity :: a
  }
  deriving (Generic, FromJSON, ToJSON)

instance ToSchema a => ToSchema (Identified a)
