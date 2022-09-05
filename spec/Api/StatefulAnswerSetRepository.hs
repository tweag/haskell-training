module Api.StatefulAnswerSetRepository where

import Api.InMemoryState
import Domain.Answer
import Domain.AnswerSetRepository
import Domain.Id
import Domain.Question
import Domain.Questionnaire

-- base
import Control.Monad.IO.Class
import Data.List hiding (filter, insert)
import Prelude hiding (filter, id)

-- containers
import Data.Map hiding (foldr)

-- microlens-platform
import Lens.Micro.Platform

-- stm
import Control.Concurrent.STM

statefulAnswerSetRepository :: TVar InMemoryState -> AnswerSetRepository IO
statefulAnswerSetRepository memory = AnswerSetRepository
  { record = statefulRecordAnswerSet memory
  , allForQuestionnaire = statefulAllAnswerSetsForQuestionnaire memory
  }

statefulRecordAnswerSet :: TVar InMemoryState -> [AnswerData] -> IO (Id AnswerSet)
statefulRecordAnswerSet memory answersData = do
  answerSetId <- liftIO generate
  answers <- traverse
    (\answer -> do
      answerId <- liftIO generate
      pure $ Identified answerId (Answer (contentData answer) (questionIdData answer) answerSetId)
    )
    answersData
  atomically . modifyTVar memory . over answersL $ (\map -> foldr (insert <$> id <*> entity) map answers)
  pure answerSetId

statefulAllAnswerSetsForQuestionnaire :: TVar InMemoryState -> Id Questionnaire -> IO [Id AnswerSet]
statefulAllAnswerSetsForQuestionnaire memory questionnaireId' = do
  state <- readTVarIO memory
  let allQuestions = view questionsL state
      questionnaireQuestions = filter ((== questionnaireId') . questionnaireId) allQuestions
      allAnswers = view answersL state
      questionnaireAnswers = filter (flip member questionnaireQuestions . questionId) allAnswers
  pure . nub $ setId <$> elems questionnaireAnswers
