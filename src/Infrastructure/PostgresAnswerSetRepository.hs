module Infrastructure.PostgresAnswerSetRepository where

import qualified Domain.Answer as Domain
import Domain.AnswerSetRepository
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

postgresAnswerSetRepository :: Connection -> AnswerSetRepository (ExceptT QueryError IO)
postgresAnswerSetRepository connection = AnswerSetRepository
  { record = postgresRecordAnswerSet connection
  , allForQuestionnaire = postgresAllAnswerSetsForQuestionnaire connection
  }

postgresRecordAnswerSet :: Connection -> [Domain.Answer] -> ExceptT QueryError IO (Id Domain.AnswerSet)
postgresRecordAnswerSet connection answers = do
  answerSetId <- liftIO generate
  dbAnswers <- traverse
    (\answer -> do
      answerId <- liftIO generate
      pure $ serializeAnswer answerSetId (Identified answerId answer)
    )
    answers
  let addAnswersQuery = DB.add DB.answerSchema (lit <$> dbAnswers)
  ExceptT $ run (statement () . insert $ addAnswersQuery) connection
  pure answerSetId

postgresAllAnswerSetsForQuestionnaire :: Connection -> Id Domain.Questionnaire -> ExceptT QueryError IO [Id Domain.AnswerSet]
postgresAllAnswerSetsForQuestionnaire connection questionnaireId =
  ExceptT $ run (statement () . select $ DB.questionnaireAnswerSets questionnaireId) connection
