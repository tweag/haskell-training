{-# LANGUAGE DerivingVia #-}

module Domain.Forms.Question where

-- rel8
import Rel8

-- text
import Data.Text

data QuestionType
  = Paragraph
  | Number
  deriving stock (Read, Show)
  deriving DBType via ReadShow QuestionType

data Question = Question
  { title :: Text
  , qtype :: QuestionType
  }