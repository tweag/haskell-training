module Api.StatefulQuestionnaireRepository where

import Api.InMemoryState
import Domain.Id
import Domain.Questionnaire
import Domain.QuestionnaireRepository

-- base
import Prelude hiding (all)

-- containers
import Data.Map

-- stm
import Control.Concurrent.STM

statefulQuestionnaireRepository
  :: TVar InMemoryState
  -> QuestionnaireRepository IO
statefulQuestionnaireRepository memory = QuestionnaireRepository
  { add = statefulAddQuestionnaire memory
  , all = statefulAllQuestionnaires memory
  }

statefulAddQuestionnaire
  :: TVar InMemoryState
  -> Questionnaire
  -> IO (Id Questionnaire)
statefulAddQuestionnaire memory questionnaire = do
  id <- generate
  atomically . modifyTVar memory . modifyQuestionnaires $ insert id questionnaire
  pure id

statefulAllQuestionnaires
  :: TVar InMemoryState
  -> IO [Identified Questionnaire]
statefulAllQuestionnaires memory = do
  state <- readTVarIO memory
  pure $ uncurry Identified <$> assocs (questionnaires state)
