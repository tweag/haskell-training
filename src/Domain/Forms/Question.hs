{-# LANGUAGE DerivingVia #-}

module Domain.Forms.Question where

-- text
import Data.Text

-- rel8
import Rel8

data QuestionType
  = Paragraph
  | Number
  deriving stock (Read, Show)
  deriving DBType via ReadShow QuestionType

data Question = Question
  { title :: Text
  , qtype :: QuestionType
  }
