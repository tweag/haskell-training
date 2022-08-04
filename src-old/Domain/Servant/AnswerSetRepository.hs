{-# LANGUAGE RankNTypes #-}

module Domain.Servant.AnswerSetRepository where

import Domain.Servant.Answer
import Domain.Servant.Id
import Domain.Servant.Questionnaire

data AnswerSetRepository m = AnswerSetRepository
  { record              :: [Answer] -> m (Id AnswerSet)
  , allForQuestionnaire :: Id Questionnaire -> m [Id AnswerSet]
  }

hoist :: (forall a. m a -> n a) -> AnswerSetRepository m -> AnswerSetRepository n
hoist f (AnswerSetRepository record allForQuestionnaire) = AnswerSetRepository
  { record              = f . record
  , allForQuestionnaire = f . allForQuestionnaire
  }
