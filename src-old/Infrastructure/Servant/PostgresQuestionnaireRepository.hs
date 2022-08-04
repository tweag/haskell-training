module Infrastructure.Servant.PostgresQuestionnaireRepository where

import Domain.Servant.Id
import Domain.Servant.Questionnaire as Domain
import Domain.Servant.QuestionnaireRepository
import Domain.Servant.QuestionnaireRepository as Questionnaire
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

postgresQuestionnaireRepository :: Connection -> QuestionnaireRepository (ExceptT QueryError IO)
postgresQuestionnaireRepository connection = QuestionnaireRepository
  { Questionnaire.add = postgresQuestionnaireAdd connection
  , Questionnaire.all = postgresQuestionnaireAll connection
  }

postgresQuestionnaireAdd :: Connection -> Domain.Questionnaire -> ExceptT QueryError IO (Id Domain.Questionnaire)
postgresQuestionnaireAdd connection questionnaire = do
  id <- liftIO generate
  let serializedQuestionnaire = serializeQuestionnaire (Identified id questionnaire)
      addQuestionnaire        = DB.add questionnaireSchema [lit serializedQuestionnaire]
  ExceptT $ run (statement () . insert $ addQuestionnaire) connection
  pure id

postgresQuestionnaireAll :: Connection -> ExceptT QueryError IO [Identified Domain.Questionnaire]
postgresQuestionnaireAll connection = do
  questionnaires <- ExceptT $ run (statement () . select $ allQuestionnaires) connection
  pure $ deserializeQuestionnaire <$> questionnaires
