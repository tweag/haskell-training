module Infrastructure.PostgresQuestionRepository where

import Domain.Id
import qualified Domain.Question as Domain
import Domain.QuestionRepository
import qualified Domain.Questionnaire as Domain
import qualified Infrastructure.Persistence as DB
import Infrastructure.Serializer

-- hasql
import Hasql.Connection
import Hasql.Session

-- rel8
import Rel8

-- transformers
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except

postgresQuestionRepository :: Connection -> QuestionRepository (ExceptT QueryError IO)
postgresQuestionRepository connection = QuestionRepository
  { add = postgresAddQuestion connection
  , allForQuestionnaire = postgresAllQuestionsForQuestionnaire connection
  }

postgresAddQuestion :: Connection -> Domain.Question -> ExceptT QueryError IO (Id Domain.Question)
postgresAddQuestion connection question = do
  questionId <- lift generate
  let serializedQuestion = serializeQuestion (Identified questionId question)
      addQuestionQuery = DB.add DB.questionSchema [lit serializedQuestion]
  ExceptT $ run (statement () . insert $ addQuestionQuery) connection
  pure questionId

postgresAllQuestionsForQuestionnaire
  :: Connection
  -> Id Domain.Questionnaire
  -> ExceptT QueryError IO [Identified Domain.Question]
postgresAllQuestionsForQuestionnaire connection questionnaireId = do
  questions <- ExceptT $ run
    (statement () . select $ DB.questionnaireQuestions questionnaireId)
    connection
  pure $ deserializeQuestion <$> questions
