{-# LANGUAGE RankNTypes #-}

module Domain.QuestionRepository where

import Domain.Id
import Domain.Question
import Domain.Questionnaire

-- hasql
import Hasql.Session

-- servant-server
import Servant.Server

-- transformers
import Control.Monad.Trans.Except

data QuestionRepository m = QuestionRepository
  { add                 :: Question
                        -> m (Id Question)
  , allForQuestionnaire :: Id Questionnaire
                        -> m [Identified Question]
  }

hoist
  :: (forall a. ExceptT QueryError IO a -> Handler a)
  -> QuestionRepository (ExceptT QueryError IO)
  -> QuestionRepository Handler
hoist f (QuestionRepository add allForQuestionnaire) = QuestionRepository
  (f . add)
  (f . allForQuestionnaire)
