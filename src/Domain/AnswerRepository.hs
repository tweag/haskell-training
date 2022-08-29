module Domain.AnswerRepository where

import Domain.Answer
import Domain.Id
import Domain.Question

data AnswerRepository m = AnswerRepository
  { allForSet      :: Id AnswerSet -> m [Identified Answer]
  , allForQuestion :: Id Question  -> m [Identified Answer]
  }
