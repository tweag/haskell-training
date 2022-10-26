{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Api.Forms where

import Api.AppServices
import Domain.Answer
import Domain.Id
import Domain.Question
import Domain.Questionnaire
import Domain.AnswerRepository as Answer
import Domain.QuestionRepository as Question
import Domain.QuestionnaireRepository as Questionnaire
import Domain.SubmissionRepository as Submission

-- base
import GHC.Generics
import Prelude hiding (all)

-- servant
import Servant.API
import Servant.API.Generic

-- servant-server
import Servant.Server.Generic

data FormsApi mode = FormsApi
  { createNewQuestionnaire
      :: mode :- "create-questionnaire"
      :> ReqBody '[JSON] Questionnaire
      :> Post '[JSON] (Id Questionnaire)
  , questionnaires
      :: mode :- "questionnaires"
      :> Get '[JSON] [Identified Questionnaire]
  , addNewQuestion
      :: mode :- "add-question"
      :> ReqBody '[JSON] Question
      :> Post '[JSON] (Id Question)
  , questionnaireQuestions
      :: mode :- "questions"
      :> Capture "questionnaire" (Id Questionnaire)
      :> Get '[JSON] [Identified Question]
  , recordSubmission
      :: mode :- "record-submission"
      :> ReqBody '[JSON] [AnswerData]
      :> Post '[JSON] (Id Submission)
  , submissions
      :: mode :- "submissions"
      :> Capture "questionnaire" (Id Questionnaire)
      :> Get  '[JSON] [Id Submission]
  , submissionAnswers
      :: mode :- "submission-answers"
      :> Capture "submission" (Id Submission)
      :> Get  '[JSON] [Identified Answer]
  , questionAnswers
      :: mode :- "question-answers"
      :> Capture "question" (Id Question)
      :> Get  '[JSON] [Identified Answer]
  }
  deriving Generic

formsServer :: AppServices -> FormsApi AsServer
formsServer (AppServices
  questionnaireRepository
  questionRepository
  submissionRepository
  answerRepository)
  = FormsApi
    { createNewQuestionnaire
        = Questionnaire.add questionnaireRepository
    , questionnaires
        = Questionnaire.all questionnaireRepository
    , addNewQuestion
        = Question.add questionRepository
    , questionnaireQuestions
        = Question.allForQuestionnaire questionRepository
    , recordSubmission
        = Submission.record submissionRepository
    , submissions
        = Submission.allForQuestionnaire submissionRepository
    , submissionAnswers
        = Answer.allForSubmission answerRepository
    , questionAnswers
        = Answer.allForQuestion answerRepository
    }
