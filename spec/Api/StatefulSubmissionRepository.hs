module Api.StatefulSubmissionRepository where

import Api.InMemoryState
import Domain.Answer
import Domain.SubmissionRepository
import Domain.Id
import Domain.Question
import Domain.Questionnaire

-- base
import Control.Monad.IO.Class
import Data.List hiding (filter, insert)
import Prelude hiding (filter, id)

-- containers
import Data.Map hiding (foldr)

-- stm
import Control.Concurrent.STM

statefulSubmissionRepository :: TVar InMemoryState -> SubmissionRepository IO
statefulSubmissionRepository memory = SubmissionRepository
  { record = statefulRecordSubmission memory
  , allForQuestionnaire = statefulAllSubmissionsForQuestionnaire memory
  }

statefulRecordSubmission :: TVar InMemoryState -> [AnswerData] -> IO (Id Submission)
statefulRecordSubmission memory answersData = do
  submissionId <- liftIO generate
  answers <- traverse
    (\answer -> do
      answerId <- liftIO generate
      pure $ Identified answerId (Answer (contentData answer) (questionIdData answer) submissionId)
    )
    answersData
  atomically . modifyTVar memory . modifyAnswers $ (\map -> foldr (insert <$> id <*> entity) map answers)
  pure submissionId

statefulAllSubmissionsForQuestionnaire :: TVar InMemoryState -> Id Questionnaire -> IO [Id Submission]
statefulAllSubmissionsForQuestionnaire memory questionnaireId' = do
  state <- readTVarIO memory
  let allQuestions = questions state
      questionnaireQuestions = filter ((== questionnaireId') . questionnaireId) allQuestions
      allAnswers = answers state
      questionnaireAnswers = filter (flip member questionnaireQuestions . questionId) allAnswers
  pure . nub $ submissionId <$> elems questionnaireAnswers
