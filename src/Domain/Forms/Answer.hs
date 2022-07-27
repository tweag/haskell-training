{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Domain.Forms.Answer where

-- rel8
import Rel8

-- text
import Data.Text

-- uuid
import Data.UUID

newtype SetId = SetId UUID
  deriving newtype (DBType, DBEq, Show)

data Content
  = ParagraphAnswer Text
  | NumberAnswer Int
  deriving stock (Read, Show)
  deriving (DBType, DBEq) via ReadShow Content

data Answer = Answer
  { setId   :: SetId
  , content :: Content
  }
