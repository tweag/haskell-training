module Domain.Servant.QuestionnaireRepository where

import Domain.Servant.Id
import Domain.Servant.Questionnaire

data QuestionnaireRepository m = QuestionnaireRepository
  { add :: Questionnaire -> m (Id Questionnaire)
  , all :: m [Identified Questionnaire]
  }