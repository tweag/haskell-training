module Domain.Forms.Answer where

-- text
import Data.Text

-- uuid
import Data.UUID

newtype SetId = SetId UUID

data AnswerContent
  = ParagraphAnswer Text
  | NumberAnswer Int

data Answer = Answer
  { setId :: SetId
  , content :: AnswerContent
  }
