{-# LANGUAGE FlexibleContexts #-}

module Infrastructure.PostgresQuestionRepository where

import Domain.Forms.Id
import qualified Domain.Forms.Question as Domain
import qualified Domain.Forms.Questionnaire as Domain
import Domain.Forms.QuestionRepository
import Infrastructure.Database
import Infrastructure.Serializer

-- base
import Control.Monad.IO.Class

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


postgresQuestionRepository :: (Monad m, MonadIO m, MonadError QueryError m) => Connection -> QuestionRepository m
postgresQuestionRepository connection = QuestionRepository
  { addNewQuestion                  = postgresAddNewQuestion connection
  , selectAllQuestionnaireQuestions = postgresSelectAllQuestionnaireQuestions connection
  }

postgresAddNewQuestion :: (Monad m, MonadIO m, MonadError QueryError m) => Connection -> Id Domain.Questionnaire -> Domain.Question -> m (Id Domain.Question)
postgresAddNewQuestion connection questionnaireId question = do
  questionId <- liftIO nextRandom
  eitherErrorUnit <- liftIO $ run (statement () . insert $ add questionSchema [lit $ serializeQuestion (Id questionId) questionnaireId question]) connection
  liftEither $ Id questionId <$ eitherErrorUnit

postgresSelectAllQuestionnaireQuestions :: (Monad m, MonadIO m, MonadError QueryError m) => Connection -> Id Domain.Questionnaire -> m [Domain.Question]
postgresSelectAllQuestionnaireQuestions connection questionnaireId = do
  eitherErrorQuestions <- liftIO $ run (statement () . select $ questionnaireQuestions questionnaireId) connection
  liftEither $ fmap (thd3 . deserializeQuestion) <$> eitherErrorQuestions
