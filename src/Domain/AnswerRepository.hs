{-# LANGUAGE RankNTypes #-}

module Domain.AnswerRepository where

import Domain.Answer
import Domain.Id
import Domain.Question

-- hasql
import Hasql.Session

-- servant-server
import Servant.Server

-- transformers
import Control.Monad.Trans.Except

data AnswerRepository m = AnswerRepository
  { allForSubmission :: Id Submission -> m [Identified Answer]
  , allForQuestion   :: Id Question   -> m [Identified Answer]
  }

hoist
  :: (forall a. ExceptT QueryError IO a -> Handler a)
  -> AnswerRepository (ExceptT QueryError IO)
  -> AnswerRepository Handler
hoist f (AnswerRepository allForSubmission allForQuestion) = AnswerRepository
  (f . allForSubmission)
  (f . allForQuestion)
