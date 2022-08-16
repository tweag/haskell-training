module Infrastructure.PostgresQuestionRepository where

import Domain.Id
import qualified Domain.Question as Domain
import Domain.QuestionRepository
import qualified Domain.Questionnaire as Domain

-- hasql
import Hasql.Connection
import Hasql.Session

-- transformers
import Control.Monad.Trans.Except

postgresQuestionRepository :: Connection -> QuestionRepository (ExceptT QueryError IO)
postgresQuestionRepository connection = QuestionRepository
  { add = postgresAddQuestion connection
  , allForQuestionnaire = postgresAllQuestionsForQuestionnaire connection
  }

postgresAddQuestion :: Connection -> Domain.Question -> ExceptT QueryError IO (Id Domain.Question)
postgresAddQuestion = _

postgresAllQuestionsForQuestionnaire :: Connection -> Id Domain.Questionnaire -> ExceptT QueryError IO [Identified Domain.Question]
postgresAllQuestionsForQuestionnaire = _
