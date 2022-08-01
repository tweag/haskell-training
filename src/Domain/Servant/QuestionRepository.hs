module Domain.Servant.QuestionRepository where

import Domain.Servant.Id
import Domain.Servant.Question
import Domain.Servant.Questionnaire

data QuestionRepository m = QuestionRepository
  { add                 :: Question -> m (Id Question)
  , allForQuestionnaire :: Id Questionnaire -> m [Identified Question]
  }
