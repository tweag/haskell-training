module Infrastructure.Servant.PostgresAnswerSetRepository where

import qualified Domain.Servant.Answer as Domain
import Domain.Servant.AnswerSetRepository
import Domain.Servant.Id
import qualified Domain.Servant.Questionnaire as Domain
import Infrastructure.Servant.Persistence as DB
import Infrastructure.Servant.Serializer

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
  { record              = postgresRecordAnswerSet connection
  , allForQuestionnaire = postgresAllAnswerSetsForQuestionnaire connection
  }

postgresRecordAnswerSet :: Connection -> [Domain.Answer] -> ExceptT QueryError IO (Id Domain.AnswerSet)
postgresRecordAnswerSet connection answers = do
  answerSetId <- liftIO generate
  dbAnswers <- liftIO $ traverse
    (\answer -> do
      answerId <- generate
      pure $ Identified answerId answer)
    answers
  let addQuestionQuery = DB.add answerSchema $ lit . serializeAnswer <$> dbAnswers
  ExceptT $ run (statement () . insert $ addQuestionQuery) connection
  pure answerSetId

postgresAllAnswerSetsForQuestionnaire :: Connection -> Id Domain.Questionnaire -> ExceptT QueryError IO [Id Domain.AnswerSet]
postgresAllAnswerSetsForQuestionnaire connection questionnaireId =
  ExceptT $ run (statement () . select $ questionnaireAnswerSets questionnaireId) connection
