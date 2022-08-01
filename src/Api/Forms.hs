{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Api.Forms where

import Api.AppServices
import Domain.Servant.Answer
import Domain.Servant.AnswerRepository as Answer
import Domain.Servant.AnswerSetRepository as AnswerSet
import Domain.Servant.Id
import Domain.Servant.Question
import Domain.Servant.QuestionRepository as Question
import Domain.Servant.Questionnaire
import Domain.Servant.QuestionnaireRepository as Questionnaires

-- servant
import Servant.API
import Servant.API.Generic

-- servant-server
import Servant.Server.Generic
import Servant.Server.Internal.Handler

data FormsApi mode = FormsApi
  { createNewQuestionnaire :: mode :- "create-questionnaire" :> ReqBody '[JSON] Questionnaire              :> Post '[JSON] (Id Questionnaire)
  , questionnaires         :: mode :- "questionnaires"                                                     :> Get  '[JSON] [Identified Questionnaire]
  , addNewQuestion         :: mode :- "add-question"         :> ReqBody '[JSON] Question                   :> Post '[JSON] (Id Question)
  , questionnaireQuestions :: mode :- "questions"            :> Capture "questionnaire" (Id Questionnaire) :> Get  '[JSON] [Identified Question]
  , recordAnswerSet        :: mode :- "record-answer-set"    :> ReqBody '[JSON] [Answer]                   :> Post '[JSON] (Id AnswerSet)
  , answerSets             :: mode :- "answer-sets"          :> Capture "questionnaire" (Id Questionnaire) :> Get  '[JSON] [Id AnswerSet]
  , setIdAnswers           :: mode :- "set-answers"          :> Capture "set" (Id AnswerSet)               :> Get  '[JSON] [Answer]
  , questionAnswers        :: mode :- "question-answers"     :> Capture "question" (Id Question)           :> Get  '[JSON] [Answer]
  }
  deriving Generic

formsServer :: AppServices -> FormsApi AsServer
formsServer (AppServices questionnaireRepository questionRepository answerSetRepository answerRepository) = FormsApi
  { createNewQuestionnaire = createNewQuestionnaireHandler questionnaireRepository
  , questionnaires         = questionnairesHandler questionnaireRepository
  , addNewQuestion         = addNewQuestionHandler questionRepository
  , questionnaireQuestions = questionnaireQuestionsHandler questionRepository
  , recordAnswerSet        = recordAnswerSetHandler answerSetRepository
  , answerSets             = answerSetsHandler answerSetRepository
  , setIdAnswers           = setIdAnswersHandler answerRepository
  , questionAnswers        = questionAnswersHandler answerRepository
  }

createNewQuestionnaireHandler :: QuestionnaireRepository Handler -> Questionnaire -> Handler (Id Questionnaire)
createNewQuestionnaireHandler = Questionnaires.add

questionnairesHandler :: QuestionnaireRepository Handler -> Handler [Identified Questionnaire]
questionnairesHandler = Questionnaires.all

addNewQuestionHandler :: QuestionRepository Handler -> Question -> Handler (Id Question)
addNewQuestionHandler = Question.add

questionnaireQuestionsHandler :: QuestionRepository Handler -> Id Questionnaire -> Handler [Identified Question]
questionnaireQuestionsHandler = Question.allForQuestionnaire

recordAnswerSetHandler :: AnswerSetRepository Handler -> [Answer] -> Handler (Id AnswerSet)
recordAnswerSetHandler = AnswerSet.record

answerSetsHandler :: AnswerSetRepository Handler -> Id Questionnaire -> Handler [Id AnswerSet]
answerSetsHandler = AnswerSet.allForQuestionnaire

setIdAnswersHandler :: AnswerRepository Handler -> Id AnswerSet -> Handler [Answer]
setIdAnswersHandler = Answer.allForSet

questionAnswersHandler :: AnswerRepository Handler -> Id Question -> Handler [Answer]
questionAnswersHandler = Answer.allForQuestion
