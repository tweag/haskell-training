{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Domain.Servant.Id where

-- rel8
import Rel8

--uuid
import Data.UUID
import Data.UUID.V4

newtype Id a = Id UUID
  deriving newtype (DBType, DBEq, Show, Eq, Ord)

generate :: IO (Id a)
generate = Id <$> nextRandom

data Identified a = Identified
  { id     :: Id a
  , entity :: a
  }
