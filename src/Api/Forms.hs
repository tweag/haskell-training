{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Api.Forms where

-- servant
import Servant.API
import Servant.API.Generic

-- data FormsApi mode = FormsApi
--   { createNewQuestionnaire :: mode :- "create-questionnaire" :> ReqBody '[JSON] Questionnaire :> Post '[JSON] (Id Questionnaire)
--   , questionnaires         :: mode :- _
--   , addNewQuestion         :: mode :- _
--   , questionnaireQuestions :: mode :- _
--   , recordAnswerSet        :: mode :- _
--   , answerSets             :: mode :- _
--   , setIdAnswers           :: mode :- _
--   , questionAnswers        :: mode :- _
--   }
