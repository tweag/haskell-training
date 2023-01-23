{-# LANGUAGE RankNTypes #-}

module Domain.AnswerRepository where

import Domain.Answer
import Domain.Id
import Domain.Question

data AnswerRepository m = AnswerRepository
  { allForSubmission :: Id Submission -> m [Identified Answer]
  , allForQuestion   :: Id Question   -> m [Identified Answer]
  }

hoist
  :: (forall a. m a -> n a)
  -> AnswerRepository m
  -> AnswerRepository n
hoist f (AnswerRepository allForSubmission allForQuestion) = AnswerRepository
  (f . allForSubmission)
  (f . allForQuestion)
