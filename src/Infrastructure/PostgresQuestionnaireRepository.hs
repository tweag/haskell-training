module Infrastructure.PostgresQuestionnaireRepository where

import Domain.Id
import qualified Domain.Questionnaire as Domain
import Domain.QuestionnaireRepository
import qualified Infrastructure.Persistence as DB
import Infrastructure.Serializer

-- base
import Prelude hiding (all)

-- hasql
import Hasql.Connection
import Hasql.Session

-- rel8
import Rel8

-- transformers
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except

postgresQuestionnaireRepository :: Connection -> QuestionnaireRepository (ExceptT QueryError IO)
postgresQuestionnaireRepository connection = QuestionnaireRepository
  { add = postgresAddQuestionnaire connection
  , all = postgresAllQuestionnaires connection
  }

postgresAddQuestionnaire
  :: Connection
  -> Domain.Questionnaire
  -> ExceptT QueryError IO (Id Domain.Questionnaire)
postgresAddQuestionnaire connection questionnaire = do
  id <- lift generate
  let serializedQuestionnaire = serializeQuestionnaire
        (Identified id questionnaire)
      addQuestionnaire = DB.add
        DB.questionnaireSchema
        [lit serializedQuestionnaire]
  ExceptT $ run
    (statement () . insert $ addQuestionnaire)
    connection
  pure id

postgresAllQuestionnaires
  :: Connection
  -> ExceptT QueryError IO [Identified Domain.Questionnaire]
postgresAllQuestionnaires connection = do
  questionnaires <- ExceptT $ run
    (statement () . select $ DB.allQuestionnaires)
    connection
  pure $ deserializeQuestionnaire <$> questionnaires
