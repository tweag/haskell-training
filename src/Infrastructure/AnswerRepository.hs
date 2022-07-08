{-# LANGUAGE FlexibleContexts #-}

module Infrastructure.AnswerRepository where

import Domain.Forms.AnswerRepository

import qualified Domain.Forms.Answer as Domain
import Domain.Forms.Id
import qualified Domain.Forms.Question as Domain
import Infrastructure.Database
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

-- mtl
import Control.Monad.Except

-- rel8
import Rel8

-- uuid
import Data.UUID.V4 (nextRandom)


postgresAnswerRepository :: (Monad m, MonadIO m, MonadError QueryError m) => Connection -> AnswerRepository m
postgresAnswerRepository connection = AnswerRepository
  { addAnswers              = postgresAddAnswers connection
  , selectAnswersBySetId    = postgresSelectAnswersBySetId connection
  , selectAnswersByQuestion = postgersSelectAnswersByQuestion connection
  }

postgresAddAnswers :: (Monad m, MonadIO m, MonadError QueryError m) => Connection -> Map (Id Domain.Question) Domain.Answer -> m (Map (Id Domain.Question) (Id Domain.Answer))
postgresAddAnswers connection answers = do
  dbAnswers <- traverseWithKey
    (\questionId answer -> do
      answerId <- liftIO $ Id <$> nextRandom
      pure $ serializeAnswer answerId questionId answer
    )
    answers
  eitherErrorUnit <- liftIO $ run (statement () . Rel8.insert $ add answerSchema (lit <$>  elems dbAnswers)) connection
  liftEither $ fmap answerId dbAnswers <$ eitherErrorUnit

filterToQuery :: Filter -> Query (Answer Expr)
filterToQuery AllQuestionnaires                     = each answerSchema
filterToQuery (SingleQuestionnaire questionnaireId) = questionnaireAnswers questionnaireId

postgresSelectAnswersBySetId :: (Monad m, MonadIO m, MonadError QueryError m) => Connection -> Filter -> m (Map Domain.SetId [Domain.Answer])
postgresSelectAnswersBySetId connection filter = do
  eitherErrorAnswers <- liftIO $ run (statement () . select $ filterToQuery filter `groupedBy` answerSetId) connection
  liftEither $ fmap (fmap $ thd3 . deserializeAnswer) . fromList <$> eitherErrorAnswers

postgersSelectAnswersByQuestion :: (Monad m, MonadIO m, MonadError QueryError m) => Connection -> Filter -> m (Map (Id Domain.Question) [Domain.Answer])
postgersSelectAnswersByQuestion connection filter = do
  eitherErrorAnswers <- liftIO $ run (statement () . select $ filterToQuery filter `groupedBy` answerQuestionId) connection
  liftEither $ fmap (fmap $ thd3 . deserializeAnswer) . fromList <$> eitherErrorAnswers
