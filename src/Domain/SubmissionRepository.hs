{-# LANGUAGE RankNTypes #-}

module Domain.SubmissionRepository where

import Domain.Answer
import Domain.Id
import Domain.Questionnaire

data SubmissionRepository m = SubmissionRepository
  { record              :: [AnswerData]     -> m (Id Submission)
  , allForQuestionnaire :: Id Questionnaire -> m [Id Submission]
  }

hoist
  :: (forall a. m a -> n a)
  -> SubmissionRepository m
  -> SubmissionRepository n
hoist f (SubmissionRepository record allForQuestionnaire) = SubmissionRepository
  (f . record)
  (f . allForQuestionnaire)
