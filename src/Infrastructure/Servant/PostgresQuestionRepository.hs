module Infrastructure.Servant.PostgresQuestionRepository where

import Domain.Servant.Id
import Domain.Servant.Question as Domain
import Domain.Servant.Questionnaire as Domain
import Domain.Servant.QuestionRepository as Question
import Infrastructure.Servant.Persistence as DB
import Infrastructure.Servant.Serializer

-- base
import Control.Monad.IO.Class

-- rel8
import Rel8

-- transformers
import Control.Monad.Trans.Except

-- hasql
import Hasql.Connection
import Hasql.Session

postgresQuestionRepository :: Connection -> QuestionRepository (ExceptT QueryError IO)
postgresQuestionRepository connection = QuestionRepository
  { Question.add                 = postgresQuestionAdd connection
  , Question.allForQuestionnaire = postgersQuestionAddForQuestionnaire connection
  }

postgresQuestionAdd :: Connection -> Domain.Question -> ExceptT QueryError IO (Id Domain.Question)
postgresQuestionAdd connection question = do
  questionId <- liftIO generate
  let addQuestionQuery = DB.add questionSchema [lit $ serializeQuestion (Identified questionId question)]
  ExceptT $ run (statement () . insert $ addQuestionQuery) connection
  pure questionId

postgersQuestionAddForQuestionnaire :: Connection -> Id Domain.Questionnaire -> ExceptT QueryError IO [Identified Domain.Question]
postgersQuestionAddForQuestionnaire connection questionnaireId = do
  questions <- ExceptT $ run (statement () . select $ questionnaireQuestions questionnaireId) connection
  pure $ deserializeQuestion <$> questions
