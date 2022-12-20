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

-- stm
import Control.Concurrent.STM

statefulAnswerRepository :: TVar InMemoryState -> AnswerRepository IO
statefulAnswerRepository memory = AnswerRepository
  { allForSubmission = statefulAllAnswersForSubmission memory
  , allForQuestion   = statefulAllAnswersForQuestion memory
  }

statefulAllAnswersForSubmission :: TVar InMemoryState -> Id Submission -> IO [Identified Answer]
statefulAllAnswersForSubmission memory submissionId' = do
  state <- readTVarIO memory
  let submissionAnswers = filter ((== submissionId') . submissionId) (answers state)
  pure $ uncurry Identified <$> assocs submissionAnswers

statefulAllAnswersForQuestion :: TVar InMemoryState -> Id Question -> IO [Identified Answer]
statefulAllAnswersForQuestion memory questionId' = do
  state <- readTVarIO memory
  let questionAnswers = filter ((== questionId') . questionId) (answers state)
  pure $ uncurry Identified <$> assocs questionAnswers
