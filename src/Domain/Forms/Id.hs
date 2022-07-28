{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Domain.Forms.Id where

-- rel8
import Rel8

--uuid
import Data.UUID
import Data.UUID.V4

newtype Id a = Id UUID
  deriving newtype (DBType, DBEq, Show, Eq, Ord)

generate :: IO (Id a)
generate = Id <$> nextRandom
