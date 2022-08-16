module Infrastructure.PostgresQuestionRepository where

import Domain.Id
import Domain.QuestionRepository

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

postgresAllQuestionsForQuestionnaire :: Connection -> Id Domain.Questionnaire -> ExceptT QueryError IO [Identified Domain.Question]
postgresAllQuestionsForQuestionnaire = _
