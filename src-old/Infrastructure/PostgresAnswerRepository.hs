module Infrastructure.PostgresAnswerRepository where

import qualified Domain.Forms.Answer as Domain
import Domain.Forms.AnswerRepository
import Domain.Forms.Id
import qualified Domain.Forms.Question as Domain
import Infrastructure.Persistence
import Infrastructure.Serializer

-- base
import Control.Monad.IO.Class

-- containers
import Data.Map

-- extra
import Data.Tuple.Extra

-- hasql
import Hasql.Connection
import Hasql.Session

-- rel8
import Rel8

-- transformers
import Control.Monad.Trans.Except

postgresAnswerRepository :: Connection -> AnswerRepository (ExceptT QueryError IO)
postgresAnswerRepository connection = AnswerRepository
  { addAnswers              = postgresAddAnswers connection
  , selectAnswersBySetId    = postgresSelectAnswersBySetId connection
  , selectAnswersByQuestion = postgersSelectAnswersByQuestion connection
  }

postgresAddAnswers :: Connection -> Map (Id Domain.Question) Domain.Answer -> ExceptT QueryError IO (Map (Id Domain.Question) (Id Domain.Answer))
postgresAddAnswers connection answers = do
  dbAnswers <- traverseWithKey
    (\questionId answer -> do
      answerId <- liftIO generate
      pure $ serializeAnswer answerId questionId answer
    )
    answers
  ExceptT $ run (statement () . Rel8.insert $ add answerSchema (lit <$>  elems dbAnswers)) connection
  pure $ answerId <$> dbAnswers

filterToQuery :: Filter -> Query (Answer Expr)
filterToQuery AllQuestionnaires                     = each answerSchema
filterToQuery (SingleQuestionnaire questionnaireId) = questionnaireAnswers questionnaireId

postgresSelectAnswersBySetId :: Connection -> Filter -> ExceptT QueryError IO (Map Domain.SetId [Domain.Answer])
postgresSelectAnswersBySetId connection filter = do
  answers <- ExceptT $ run (statement () . select $ filterToQuery filter `groupedBy` answerSetId) connection
  pure $ fmap (thd3 . deserializeAnswer) <$> fromList answers

postgersSelectAnswersByQuestion :: Connection -> Filter -> ExceptT QueryError IO (Map (Id Domain.Question) [Domain.Answer])
postgersSelectAnswersByQuestion connection filter = do
  answers <- ExceptT $ run (statement () . select $ filterToQuery filter `groupedBy` answerQuestionId) connection
  pure $ fmap (thd3 . deserializeAnswer) <$> fromList answers