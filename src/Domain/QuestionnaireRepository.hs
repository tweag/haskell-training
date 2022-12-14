{-# LANGUAGE RankNTypes #-}

module Domain.QuestionnaireRepository where

import Domain.Id
import Domain.Questionnaire

-- hasql
import Hasql.Session

-- servant-server
import Servant.Server

-- transformers
import Control.Monad.Trans.Except

data QuestionnaireRepository m = QuestionnaireRepository
  { add :: Questionnaire -> m (Id Questionnaire)
  , all :: m [Identified Questionnaire]
  }

hoist
  :: (forall a. ExceptT QueryError IO a -> Handler a)
  -> QuestionnaireRepository (ExceptT QueryError IO)
  -> QuestionnaireRepository Handler
hoist f (QuestionnaireRepository add all) = QuestionnaireRepository
  (f . add)
  (f all)
