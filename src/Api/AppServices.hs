module Api.AppServices where

import Domain.AnswerRepository as Answer
import Domain.QuestionnaireRepository as Questionnaire
import Domain.QuestionRepository as Question
import Domain.SubmissionRepository as Submission
import Infrastructure.PostgresAnswerRepository
import Infrastructure.PostgresQuestionRepository
import Infrastructure.PostgresQuestionnaireRepository
import Infrastructure.PostgresSubmissionRepository

-- bytestring
import Data.ByteString.Lazy.Char8

-- hasql
import Hasql.Connection

-- servant-server
import Servant
import Hasql.Session

-- transformers
import Control.Monad.Trans.Except

data AppServices = AppServices
  { questionnaireRepository :: QuestionnaireRepository Handler
  , questionRepository      :: QuestionRepository Handler
  , submissionRepository    :: SubmissionRepository Handler
  , answerRepository        :: AnswerRepository Handler
  }

postgresAppServices :: Connection -> AppServices
postgresAppServices connection = AppServices
  { questionnaireRepository = Questionnaire.hoist f $
      postgresQuestionnaireRepository connection
  , questionRepository      = Question.hoist f $
      postgresQuestionRepository connection
  , submissionRepository    = Submission.hoist f $
      postgresSubmissionRepository connection
  , answerRepository        = Answer.hoist f $
      postgresAnswerRepository connection
  }
  where
    f :: ExceptT QueryError IO a -> Handler a
    f exceptT = Handler $ withExceptT
      (\queryError -> err500 {errBody = pack $ show queryError})
      exceptT
