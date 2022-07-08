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
  deriving newtype (DBType, DBEq, Eq, Ord, Show)

data AnswerContent
  = ParagraphAnswer Text
  | NumberAnswer Int
  deriving stock (Read, Show)
  deriving (DBType, DBEq) via ReadShow AnswerContent

data Answer = Answer
  { setId :: SetId
  , content :: AnswerContent
  }
