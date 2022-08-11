module Api.AppServices where

import Domain.AnswerRepository
import Domain.AnswerSetRepository
import Domain.QuestionnaireRepository
import Domain.QuestionRepository

-- servant-server
import Servant

data AppServices = AppServices
  { questionnaireRepository :: QuestionnaireRepository Handler
  , questionRepository      :: QuestionRepository Handler
  , answerSetRepository     :: AnswerSetRepository Handler
  , answerRepository        :: AnswerRepository Handler
  }
