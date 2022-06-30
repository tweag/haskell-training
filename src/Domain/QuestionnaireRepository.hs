module QuestionnaireRepository where

import Domain.Forms

data QuestionnaireRepository = QuestionnaireRepository
  { createNewQuestionnaire  :: Questionnaire -> QuestionnaireId
  , selectAllQuestionnaires :: [Questionnaire]
  }