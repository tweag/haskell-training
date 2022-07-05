module Domain.Forms.Id where

--uuid
import Data.UUID

newtype Id a = Id UUID
