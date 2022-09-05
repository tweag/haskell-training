module Api.StatefulQuestionRepository where

import Api.InMemoryState
import Domain.Id
import Domain.Question
import Domain.QuestionRepository
import Domain.Questionnaire

-- base
import Control.Monad.IO.Class
import Prelude hiding (filter)

-- containers
import Data.Map

-- microlens-platform
import Lens.Micro.Platform

-- stm
import Control.Concurrent.STM

statefulQuestionRepository :: TVar InMemoryState -> QuestionRepository IO
statefulQuestionRepository memory = QuestionRepository
  { add = statefulAddQuestion memory
  , allForQuestionnaire = statefulAllQuestionsForQuestionnaire memory
  }

statefulAddQuestion :: TVar InMemoryState -> Question -> IO (Id Question)
statefulAddQuestion memory question = do
  questionId <- liftIO generate
  atomically . modifyTVar memory $ over questionsL $ insert questionId question
  pure questionId

statefulAllQuestionsForQuestionnaire :: TVar InMemoryState -> Id Questionnaire -> IO [Identified Question]
statefulAllQuestionsForQuestionnaire memory questionnaireId' = do
  state <- readTVarIO memory
  let questionnaireQuestions = filter ((== questionnaireId') . questionnaireId) (view questionsL state)
  pure $ uncurry Identified <$> assocs questionnaireQuestions
