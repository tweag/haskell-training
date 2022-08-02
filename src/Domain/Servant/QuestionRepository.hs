{-# LANGUAGE RankNTypes #-}

module Domain.Servant.QuestionRepository where

import Domain.Servant.Id
import Domain.Servant.Question
import Domain.Servant.Questionnaire

data QuestionRepository m = QuestionRepository
  { add                 :: Question -> m (Id Question)
  , allForQuestionnaire :: Id Questionnaire -> m [Identified Question]
  }

hoist :: (forall a. m a -> n a) -> QuestionRepository m -> QuestionRepository n
hoist f (QuestionRepository add allForQuestionnaire) = QuestionRepository
  { add                 = f . add
  , allForQuestionnaire = f . allForQuestionnaire
  }
