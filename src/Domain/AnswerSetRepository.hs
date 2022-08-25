module Domain.AnswerSetRepository where

import Domain.Answer
import Domain.Id
import Domain.Questionnaire

data AnswerSetRepository m = AnswerSetRepository
  { record              :: [AnswerData]     -> m (Id AnswerSet)
  , allForQuestionnaire :: Id Questionnaire -> m [Id AnswerSet]
  }
