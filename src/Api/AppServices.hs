module Api.AppServices where

import Domain.AnswerRepository as Answer
import Domain.AnswerSetRepository as AnswerSet
import Domain.QuestionnaireRepository as Questionnaire
import Domain.QuestionRepository as Question
import Infrastructure.PostgresAnswerRepository
import Infrastructure.PostgresAnswerSetRepository
import Infrastructure.PostgresQuestionRepository
import Infrastructure.PostgresQuestionnaireRepository

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

postgresAppServices :: Connection -> AppServices
postgresAppServices connection = AppServices
  { questionnaireRepository = Questionnaire.hoist f $ postgresQuestionnaireRepository connection
  , questionRepository      = Question.hoist      f $ postgresQuestionRepository connection
  , answerSetRepository     = AnswerSet.hoist     f $ postgresAnswerSetRepository connection
  , answerRepository        = Answer.hoist        f $ postgresAnswerRepository connection
  }
  where
    f :: ExceptT QueryError IO a -> Handler a
    f exceptT = Handler $ withExceptT (\queryError -> err500 {errBody = pack $ show queryError}) exceptT
