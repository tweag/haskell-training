module Api.AppServices where

import Domain.AnswerRepository
import Domain.SubmissionRepository
import Domain.QuestionnaireRepository
import Domain.QuestionRepository

-- servant-server
import Servant

data AppServices = AppServices
  { questionnaireRepository :: QuestionnaireRepository Handler
  , questionRepository      :: QuestionRepository Handler
  , submissionRepository    :: SubmissionRepository Handler
  , answerRepository        :: AnswerRepository Handler
  }
