module Api.StatefulAppServices where

import Api.AppServices
import Api.InMemoryState
import Api.StatefulAnswerRepository
import Api.StatefulSubmissionRepository
import Api.StatefulQuestionRepository
import Api.StatefulQuestionnaireRepository
import Domain.AnswerRepository as Answer
import Domain.SubmissionRepository as Submission
import Domain.QuestionnaireRepository as Questionnaire
import Domain.QuestionRepository as Question

-- base
import Control.Monad.IO.Class

-- stm
import Control.Concurrent.STM

statefulAppServices :: TVar InMemoryState -> AppServices
statefulAppServices memory = AppServices
  { questionnaireRepository = Questionnaire.hoist liftIO $ statefulQuestionnaireRepository memory
  , questionRepository      = Question.hoist      liftIO $ statefulQuestionRepository memory
  , submissionRepository    = Submission.hoist    liftIO $ statefulSubmissionRepository memory
  , answerRepository        = Answer.hoist        liftIO $ statefulAnswerRepository memory
  }
