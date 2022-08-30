{-# LANGUAGE RankNTypes #-}

module Domain.QuestionRepository where

import Domain.Id
import Domain.Question
import Domain.Questionnaire

data QuestionRepository m = QuestionRepository
  { add                 :: Question -> m (Id Question)
  , allForQuestionnaire :: Id Questionnaire -> m [Identified Question]
  }

hoist :: (forall a. m a -> n a) -> QuestionRepository m -> QuestionRepository n
hoist f (QuestionRepository add allForQuestionnaire) = QuestionRepository
  { add                 = f . add
  , allForQuestionnaire = f . allForQuestionnaire
  }
