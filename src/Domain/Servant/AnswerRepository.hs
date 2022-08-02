{-# LANGUAGE RankNTypes #-}

module Domain.Servant.AnswerRepository where

import Domain.Servant.Answer
import Domain.Servant.Id
import Domain.Servant.Question

data AnswerRepository m = AnswerRepository
  { allForSet      :: Id AnswerSet -> m [Identified Answer]
  , allForQuestion :: Id Question -> m [Identified Answer]
  }

hoist :: (forall a. m a -> n a) -> AnswerRepository m -> AnswerRepository n
hoist f (AnswerRepository allForSet allForQuestion) = AnswerRepository
  { allForSet      = f . allForSet
  , allForQuestion = f . allForQuestion
  }
