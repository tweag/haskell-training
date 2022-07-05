module Domain.Forms.QuestionnaireRepository where

import Domain.Forms.Id
import Domain.Forms.Questionnaire

data QuestionnaireRepository m = QuestionnaireRepository
  { createNewQuestionnaire  :: Questionnaire -> m (Id Questionnaire)
  , selectAllQuestionnaires :: m [Questionnaire]
  }