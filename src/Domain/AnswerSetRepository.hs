{-# LANGUAGE RankNTypes #-}

module Domain.AnswerSetRepository where

import Domain.Answer
import Domain.Id
import Domain.Questionnaire

data AnswerSetRepository m = AnswerSetRepository
  { record              :: [AnswerData]     -> m (Id AnswerSet)
  , allForQuestionnaire :: Id Questionnaire -> m [Id AnswerSet]
  }

hoist :: (forall a. m a -> n a) -> AnswerSetRepository m -> AnswerSetRepository n
hoist f (AnswerSetRepository record allForQuestionnaire) = AnswerSetRepository
  { record              = f . record
  , allForQuestionnaire = f . allForQuestionnaire
  }
