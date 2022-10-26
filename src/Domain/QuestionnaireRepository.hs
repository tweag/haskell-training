module Domain.QuestionnaireRepository where

import Domain.Id
import Domain.Questionnaire

data QuestionnaireRepository m = QuestionnaireRepository
  { add :: Questionnaire -> m (Id Questionnaire)
  , all :: m [Identified Questionnaire]
  }
