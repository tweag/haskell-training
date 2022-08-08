module Domain.Id where

-- aeson
import Data.Aeson

--uuid
import Data.UUID

newtype Id a = Id UUID

instance ToJSON (Id a) where
  toJSON (Id uuid) = toJSON uuid

data Identified a = Identified
  { id     :: Id a
  , entity :: a
  }
