module Domain.Id where

-- uuid
import Data.UUID

newtype Id a = Id UUID
