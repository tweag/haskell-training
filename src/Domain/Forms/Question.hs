module Domain.Forms.Question where

-- text
import Data.Text

data QuestionType
  = Paragraph
  | Number

data Question = Question
  { title :: Text
  , qtype :: QuestionType
  }