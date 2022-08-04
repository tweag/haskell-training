module Infrastructure.Servant.PostgresAnswerRepository where

import qualified Domain.Servant.Answer as Domain
import Domain.Servant.AnswerRepository
import Domain.Servant.Id
import qualified Domain.Servant.Question as Domain
import Infrastructure.Servant.Persistence
import Infrastructure.Servant.Serializer

-- transformers
import Control.Monad.Trans.Except

-- hasql
import Hasql.Connection
import Hasql.Session

-- rel8
import Rel8

postgresAnswerRepository :: Connection -> AnswerRepository (ExceptT QueryError IO)
postgresAnswerRepository connection = AnswerRepository
  { allForSet      = postgresAllAnswersForSet connection
  , allForQuestion = postgresAllAnswersForQuestion connection
  }

postgresAllAnswersForSet :: Connection -> Id Domain.AnswerSet -> ExceptT QueryError IO [Identified Domain.Answer]
postgresAllAnswersForSet connection answerSetId = do
  questions <- ExceptT $ run (statement () . select $ answerSetAnswers answerSetId) connection
  pure $ deserializeAnswer <$> questions

postgresAllAnswersForQuestion :: Connection -> Id Domain.Question -> ExceptT QueryError IO [ Identified Domain.Answer]
postgresAllAnswersForQuestion connection questionId = do
  questions <- ExceptT $ run (statement () . select $ questionAnswers questionId) connection
  pure $ deserializeAnswer <$> questions
