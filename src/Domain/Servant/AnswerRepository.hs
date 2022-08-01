module Domain.Servant.AnswerRepository where

import Domain.Servant.Answer
import Domain.Servant.Id
import Domain.Servant.Question

data AnswerRepository m = AnswerRepository
  { allForSet      :: Id AnswerSet -> m [Answer]
  , allForQuestion :: Id Question -> m [Answer]
  }
