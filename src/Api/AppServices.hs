module Api.AppServices where

import Domain.Servant.AnswerRepository
import Domain.Servant.AnswerSetRepository
import Domain.Servant.QuestionnaireRepository
import Domain.Servant.QuestionRepository

-- servant-server
import Servant

data AppServices = AppServices
  { questionnaireRepository :: QuestionnaireRepository Handler
  , questionRepository      :: QuestionRepository Handler
  , answerSetRepository     :: AnswerSetRepository Handler
  , answerRepository        :: AnswerRepository Handler
  }
