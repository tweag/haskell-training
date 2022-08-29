{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Domain.Id where

-- aeson
import Data.Aeson

-- base
import GHC.Generics

-- openapi3
import Data.OpenApi

-- rel8
import Rel8

-- servant-server
import Servant

-- uuid
import Data.UUID

newtype Id a = Id UUID
  deriving newtype (Show, FromJSON, ToJSON, ToSchema, ToParamSchema, FromHttpApiData, DBType)

data Identified a = Identified
  { id     :: Id a
  , entity :: a
  }
  deriving (Generic, FromJSON, ToJSON)

instance ToSchema a => ToSchema (Identified a)
