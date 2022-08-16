module Infrastructure.PostgresAnswerRepository where

import qualified Domain.Answer as Domain
import Domain.AnswerRepository
import Domain.Id
import qualified Domain.Question as Domain
import qualified Infrastructure.Persistence as DB
import Infrastructure.Serializer

-- hasql
import Hasql.Connection
import Hasql.Session

-- rel8
import Rel8

-- transformers
import Control.Monad.Trans.Except

postgresAnswerRepository :: Connection -> AnswerRepository (ExceptT QueryError IO)
postgresAnswerRepository connection = AnswerRepository
  { allForSet = postgresAllAnswersForSet connection
  , allForQuestion = postgresAllAnswersForQuestion connection
  }

postgresAllAnswersForSet :: Connection -> Id Domain.AnswerSet -> ExceptT QueryError IO [Identified Domain.Answer]
postgresAllAnswersForSet connection answerSetId = do
  answers <- ExceptT $ run (statement () . select $ DB.answerSetAnswers answerSetId) connection
  pure $ deserializeAnswer <$> answers

postgresAllAnswersForQuestion :: Connection -> Id Domain.Question -> ExceptT QueryError IO [Identified Domain.Answer]
postgresAllAnswersForQuestion connection questionId = do
  answers <- ExceptT $ run (statement () . select $ DB.questionAnswers questionId) connection
  pure $ deserializeAnswer <$> answers
