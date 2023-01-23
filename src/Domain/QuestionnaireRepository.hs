{-# LANGUAGE RankNTypes #-}

module Domain.QuestionnaireRepository where

import Domain.Id
import Domain.Questionnaire

data QuestionnaireRepository m = QuestionnaireRepository
  { add :: Questionnaire -> m (Id Questionnaire)
  , all :: m [Identified Questionnaire]
  }

hoist
  :: (forall a. m a -> n a)
  -> QuestionnaireRepository m
  -> QuestionnaireRepository n
hoist f (QuestionnaireRepository add all) = QuestionnaireRepository
  (f . add)
  (f all)
