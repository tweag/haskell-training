module Infrastructure.PostgresSubmissionRepository where

import qualified Domain.Answer as Domain
import Domain.SubmissionRepository
import Domain.Id
import qualified Domain.Questionnaire as Domain
import qualified Infrastructure.Persistence as DB
import Infrastructure.Serializer

-- base
import Control.Monad.IO.Class

-- hasql
import Hasql.Connection
import Hasql.Session

-- rel8
import Rel8

-- transformers
import Control.Monad.Trans.Except

postgresSubmissionRepository :: Connection -> SubmissionRepository (ExceptT QueryError IO)
postgresSubmissionRepository connection = SubmissionRepository
  { record = postgresRecordSubmission connection
  , allForQuestionnaire = postgresAllSubmissionsForQuestionnaire connection
  }

postgresRecordSubmission
  :: Connection
  -> [Domain.AnswerData]
  -> ExceptT QueryError IO (Id Domain.Submission)
postgresRecordSubmission connection answers = do
  submissionId <- liftIO generate
  dbAnswers <- traverse
    (\answer -> do
      answerId <- liftIO generate
      pure $ serializeAnswer submissionId (Identified answerId answer)
    )
    answers
  let addAnswersQuery = DB.add DB.answerSchema (lit <$> dbAnswers)
  ExceptT $ run (statement () . insert $ addAnswersQuery) connection
  pure submissionId

postgresAllSubmissionsForQuestionnaire
  :: Connection
  -> Id Domain.Questionnaire
  -> ExceptT QueryError IO [Id Domain.Submission]
postgresAllSubmissionsForQuestionnaire connection questionnaireId =
  ExceptT $ run
    (statement () . select $ DB.questionnaireSubmissions questionnaireId)
    connection
