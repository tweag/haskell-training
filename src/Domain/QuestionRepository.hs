module Domain.QuestionRepository where

import Domain.Id
import Domain.Question
import Domain.Questionnaire

data QuestionRepository m = QuestionRepository
  { add                 :: Question -> m (Id Question)
  , allForQuestionnaire :: Id Questionnaire -> m [Identified Question]
  }
