module Domain.Servant.Answer where

import Domain.Servant.Id
import Domain.Servant.Question

-- text
import Data.Text

data Content
  = ParagraphAnswer Text
  | NumberAnswer Int

data AnswerSet = AnswerSet

data Answer = Answer
  { content :: Content
  , questionId :: Id Question
  , setId :: Id AnswerSet
  }
