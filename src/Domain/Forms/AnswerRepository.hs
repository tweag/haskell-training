module Domain.Forms.AnswerRepository where

import Domain.Forms.Answer
import Domain.Forms.Id
import Domain.Forms.Question
import Domain.Forms.Questionnaire

-- containers
import Data.Map

data GroupBy
  = GroupByQuestion
  | GroupBySet

data AnswerRepository m = AnswerRepository
  { addAnswers    :: Map (Id Question) Answer -> m (Map (Id Question) (Id Answer))
  , selectAnswers :: Maybe (Id Questionnaire) -> GroupBy -> m [Answer]
  }
