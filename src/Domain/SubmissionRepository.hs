{-# LANGUAGE RankNTypes #-}

module Domain.SubmissionRepository where

import Domain.Answer
import Domain.Id
import Domain.Questionnaire

-- hasql
import Hasql.Session

-- servant-server
import Servant.Server

-- transformers
import Control.Monad.Trans.Except

data SubmissionRepository m = SubmissionRepository
  { record              :: [AnswerData]     -> m (Id Submission)
  , allForQuestionnaire :: Id Questionnaire -> m [Id Submission]
  }

hoist
  :: (forall a. ExceptT QueryError IO a -> Handler a)
  -> SubmissionRepository (ExceptT QueryError IO)
  -> SubmissionRepository Handler
hoist f (SubmissionRepository record allForQuestionnaire) = SubmissionRepository
  (f . record)
  (f . allForQuestionnaire)
