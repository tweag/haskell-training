module Infrastructure.PostgresQuestionnaireRepository where

import Domain.Forms.Id
import qualified Domain.Forms.Questionnaire as Domain
import Domain.Forms.QuestionnaireRepository
import Infrastructure.Persistence
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

postgresQuestionnaireRepository :: Connection -> QuestionnaireRepository (ExceptT QueryError IO)
postgresQuestionnaireRepository connection = QuestionnaireRepository
  { createNewQuestionnaire  = postgresCreateNewQuestionnaire connection
  , selectAllQuestionnaires = postgresSelectAllQuestionnaires connection
  }

postgresCreateNewQuestionnaire :: Connection -> Domain.Questionnaire -> ExceptT QueryError IO (Id Domain.Questionnaire)
postgresCreateNewQuestionnaire connection questionnaire = do
  id <- liftIO generate
  let serializedQuestionnaire = serializeQuesionnaire id questionnaire
      addQuestionnaire        = add questionnaireSchema [lit serializedQuestionnaire]
  ExceptT $ run (statement () . insert $ addQuestionnaire) connection
  pure id

postgresSelectAllQuestionnaires :: Connection -> ExceptT QueryError IO [Domain.Questionnaire]
postgresSelectAllQuestionnaires connection = do
  questionnaires <- ExceptT $ run (statement () . select $ allQuestionnaires) connection
  pure $ snd . deserializeQuestionnaire <$> questionnaires
