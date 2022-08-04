-- {-# LANGUAGE DeriveAnyClass #-}
-- {-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}

module Domain.Servant.Id where

-- aeson
import Data.Aeson
import Data.Aeson.Types

-- -- base
-- import GHC.Generics

-- rel8
import Rel8

-- servant

import Servant

--uuid
import Data.UUID
import Data.UUID.V4

newtype Id a = Id UUID
  deriving newtype (DBType, DBEq, Show, Eq, Ord, FromJSON, ToJSON, FromHttpApiData)

generate :: IO (Id a)
generate = Id <$> nextRandom

data Identified a = Identified
  { id     :: Id a
  , entity :: a
  }
  -- deriving stock Generic
  -- deriving anyclass FromJSON

instance (FromJSON a) =>  FromJSON (Identified a) where
  parseJSON :: Value -> Parser (Identified a)
  parseJSON = withObject "Identified" (\i -> Identified
    <$> i .: "id"
    <*> i .: "entity")

instance ToJSON a => ToJSON (Identified a) where
  toJSON :: Identified a -> Value
  toJSON (Identified id entity) = object
    [ "id"     .= id
    , "entity" .= entity
    ]
