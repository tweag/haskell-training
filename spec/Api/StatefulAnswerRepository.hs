module Api.StatefulAnswerRepository where

import Api.InMemoryState
import Domain.Answer
import Domain.AnswerRepository
import Domain.Id
import Domain.Question

-- base
import Prelude hiding (filter)

-- containers
import Data.Map

-- microlens-platform
import Lens.Micro.Platform

-- stm
import Control.Concurrent.STM

statefulAnswerRepository :: TVar InMemoryState -> AnswerRepository IO
statefulAnswerRepository memory = AnswerRepository
  { allForSet = statefulAllAnswersForSet memory
  , allForQuestion = statefulAllAnswersForQuestion memory
  }

statefulAllAnswersForSet :: TVar InMemoryState -> Id AnswerSet -> IO [Identified Answer]
statefulAllAnswersForSet memory answerSetId = do
  state <- readTVarIO memory
  let setAnswers = filter ((== answerSetId) . setId) (view answersL state)
  pure $ uncurry Identified <$> assocs setAnswers

statefulAllAnswersForQuestion :: TVar InMemoryState -> Id Question -> IO [Identified Answer]
statefulAllAnswersForQuestion memory questionId' = do
  state <- readTVarIO memory
  let setAnswers = filter ((== questionId') . questionId) (view answersL state)
  pure $ uncurry Identified <$> assocs setAnswers
