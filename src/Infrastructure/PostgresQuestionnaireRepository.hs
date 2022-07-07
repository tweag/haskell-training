{-# LANGUAGE FlexibleContexts #-}
module Infrastructure.PostgresQuestionnaireRepository where

import Domain.Forms.Id
import qualified Domain.Forms.Questionnaire as Domain
import Domain.Forms.QuestionnaireRepository
import Infrastructure.Database

-- base
import Control.Monad.IO.Class

-- hasql
import Hasql.Connection
import Hasql.Session

-- mtl
import Control.Monad.Except

-- rel8
import Rel8
import Infrastructure.Serializer

-- uuid
import Data.UUID.V4

postgresQuestionnaireRepository :: (Monad m, MonadIO m, MonadError QueryError m) => Connection -> QuestionnaireRepository m
postgresQuestionnaireRepository connection = QuestionnaireRepository
  { createNewQuestionnaire  = postgresCreateNewQuestionnaire connection
  , selectAllQuestionnaires = postgresSelectAllQuestionnaires connection
  }

postgresCreateNewQuestionnaire :: (Monad m, MonadIO m, MonadError QueryError m) => Connection -> Domain.Questionnaire -> m (Id Domain.Questionnaire)
postgresCreateNewQuestionnaire connection questionnaire = do
  questionnaireId <- liftIO $ Id <$> nextRandom
  eitherError <- liftIO $ run (statement () . insert $ add questionnaireSchema [lit $ serializeQuesionnaire questionnaireId questionnaire]) connection
  liftEither $ questionnaireId <$ eitherError

postgresSelectAllQuestionnaires :: (Monad m, MonadIO m, MonadError QueryError m) => Connection -> m [Domain.Questionnaire]
postgresSelectAllQuestionnaires connection = do
  eitherErrorQuestionnaires <- liftIO $ run (statement () . select $ allQuestionnaires) connection
  liftEither $ fmap (snd . deserializeQuestionnaire) <$> eitherErrorQuestionnaires
