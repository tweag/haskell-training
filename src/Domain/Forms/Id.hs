{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Domain.Forms.Id where

-- rel8
import Rel8

--uuid
import Data.UUID

newtype Id a = Id UUID
  deriving newtype (DBType, DBEq, Show)
