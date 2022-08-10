{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Api.Forms where

import Domain.Answer
import Domain.Id
import Domain.Question
import Domain.Questionnaire

-- base
import GHC.Generics

-- servant
import Servant.API
import Servant.API.Generic

data FormsApi mode = FormsApi
  { createNewQuestionnaire :: mode :- "create-questionnaire" :> ReqBody '[JSON] Questionnaire              :> Post '[JSON] (Id Questionnaire)
  , questionnaires         :: mode :- "questionnaires"                                                     :> Get  '[JSON] [Identified Questionnaire]
  , addNewQuestion         :: mode :- "add-question"         :> ReqBody '[JSON] Question                   :> Post '[JSON] (Id Question)
  , questionnaireQuestions :: mode :- "questions"            :> Capture "questionnaire" (Id Questionnaire) :> Get  '[JSON] [Identified Question]
  , recordAnswerSet        :: mode :- "record-answer-set"    :> ReqBody '[JSON] [Answer]                   :> Post '[JSON] (Id AnswerSet)
  , answerSets             :: mode :- "answer-sets"          :> Capture "questionnaire" (Id Questionnaire) :> Get  '[JSON] [Id AnswerSet]
  , setIdAnswers           :: mode :- "set-answers"          :> Capture "set" (Id AnswerSet)               :> Get  '[JSON] [Identified Answer]
  , questionAnswers        :: mode :- "question-answers"     :> Capture "question" (Id Question)           :> Get  '[JSON] [Identified Answer]
  }
  deriving Generic
