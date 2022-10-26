module Domain.Id where

-- uuid
import Data.UUID

newtype Id a = Id UUID

data Identified a = Identified
  { id     :: Id a
  , entity :: a
  }
