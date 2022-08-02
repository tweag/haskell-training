{-# LANGUAGE RankNTypes #-}

module Domain.Servant.QuestionnaireRepository where

import Domain.Servant.Id
import Domain.Servant.Questionnaire

-- base
import Prelude hiding (all)

data QuestionnaireRepository m = QuestionnaireRepository
  { add :: Questionnaire -> m (Id Questionnaire)
  , all :: m [Identified Questionnaire]
  }

hoist :: (forall a. m a -> n a) -> QuestionnaireRepository m -> QuestionnaireRepository n
hoist f (QuestionnaireRepository add all) = QuestionnaireRepository
  { add = f . add
  , all = f all
  }