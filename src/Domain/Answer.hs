module Domain.Answer where

import Domain.Id
import Domain.Question

-- text
import Data.Text

data Submission

data Answer = Answer
  { content      :: Content
  , questionId   :: Id Question
  , submissionId :: Id Submission
  }

data AnswerData = AnswerData
  { contentData    :: Content
  , questionIdData :: Id Question
  }

data Content
  = Paragraph Text
  | Number Int
