module Domain.Forms.AnswerRepository where

import Domain.Forms.Answer
import Domain.Forms.Id
import Domain.Forms.Question
import Domain.Forms.Questionnaire

-- containers
import Data.Map

data Filter
  = AllQuestionnaires
  | SingleQuestionnaire (Id Questionnaire)

data AnswerRepository m = AnswerRepository
  { addAnswers              :: Map (Id Question) Answer -> m (Map (Id Question) (Id Answer))
  , selectAnswersBySetId    :: Filter -> m (Map SetId [Answer])
  , selectAnswersByQuestion :: Filter -> m (Map (Id Question) [Answer])
  }
