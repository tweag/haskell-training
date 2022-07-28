module Infrastructure.PostgresQuestionRepository where

import Domain.Forms.Id
import qualified Domain.Forms.Question as Domain
import qualified Domain.Forms.Questionnaire as Domain
import Domain.Forms.QuestionRepository
import Infrastructure.Persistence
import Infrastructure.Serializer

-- base
import Control.Monad.IO.Class

-- extra
import Data.Tuple.Extra

-- hasql
import Hasql.Connection
import Hasql.Session

-- rel8
import Rel8

-- transformers
import Control.Monad.Trans.Except

postgresQuestionRepository :: Connection -> QuestionRepository (ExceptT QueryError IO)
postgresQuestionRepository connection = QuestionRepository
  { addNewQuestion                  = postgresAddNewQuestion connection
  , selectAllQuestionnaireQuestions = postgresSelectAllQuestionnaireQuestions connection
  }

postgresAddNewQuestion :: Connection -> Id Domain.Questionnaire -> Domain.Question -> ExceptT QueryError IO (Id Domain.Question)
postgresAddNewQuestion connection questionnaireId question = do
  questionId <- liftIO generate
  let addQuestionQuery = add questionSchema [lit $ serializeQuestion questionId questionnaireId question]
  ExceptT $ run (statement () . insert $ addQuestionQuery) connection
  pure questionId

postgresSelectAllQuestionnaireQuestions :: Connection -> Id Domain.Questionnaire -> ExceptT QueryError IO [Domain.Question]
postgresSelectAllQuestionnaireQuestions connection questionnaireId = do
  questions <- ExceptT $ run (statement () . select $ questionnaireQuestions questionnaireId) connection
  pure $ thd3 . deserializeQuestion <$> questions
