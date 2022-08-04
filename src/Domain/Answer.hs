module Domain.Answer where

import Domain.Id
import Domain.Question

-- text
import Data.Text

data Answer = Answer
  { content    :: Content
  , setId      :: Id AnswerSet
  , questionId :: Id Question
  }

data Content
  = Paragraph Text
  | Number Int

data AnswerSet = AnswerSet
