{-# LANGUAGE OverloadedStrings #-}

module Api.AppServices where

import Domain.Servant.AnswerRepository as Answer
import Domain.Servant.AnswerSetRepository as AnswerSet
import Domain.Servant.QuestionnaireRepository as Questionnaire
import Domain.Servant.QuestionRepository as Question
import Infrastructure.Servant.PostgresAnswerRepository
import Infrastructure.Servant.PostgresAnswerSetRepository
import Infrastructure.Servant.PostgresQuestionnaireRepository
import Infrastructure.Servant.PostgresQuestionRepository

-- bytestring
import Data.ByteString.Lazy.Char8

-- hasql
import Hasql.Connection
import Hasql.Session

-- servant-server
import Servant

-- transformers
import Control.Monad.Trans.Except
data AppServices = AppServices
  { questionnaireRepository :: QuestionnaireRepository Handler
  , questionRepository      :: QuestionRepository Handler
  , answerSetRepository     :: AnswerSetRepository Handler
  , answerRepository        :: AnswerRepository Handler
  }

buildAppServices :: Connection -> AppServices
buildAppServices connection = AppServices
  { questionnaireRepository = Questionnaire.hoist exceptTToHandler $ postgresQuestionnaireRepository connection
  , questionRepository      = Question.hoist exceptTToHandler $ postgresQuestionRepository connection
  , answerSetRepository     = AnswerSet.hoist exceptTToHandler $ postgresAnswerSetRepository connection
  , answerRepository        = Answer.hoist exceptTToHandler $postgresAnswerRepository connection
  }
  where
    exceptTToHandler :: ExceptT QueryError IO a -> Handler a
    exceptTToHandler exceptT = Handler $ withExceptT (\queryError -> err500 {errBody = pack $ show queryError}) exceptT
