module Api.InMemoryState where

import Domain.Answer
import Domain.Id
import Domain.Question
import Domain.Questionnaire

-- containers
import Data.Map

data InMemoryState = InMemoryState
  { questionnaires :: Map (Id Questionnaire) Questionnaire
  , questions      :: Map (Id Question)      Question
  , answers        :: Map (Id Answer)        Answer
  }

modifyQuestionnaires
  :: (   Map (Id Questionnaire) Questionnaire
      -> Map (Id Questionnaire) Questionnaire)
  -> InMemoryState -> InMemoryState
modifyQuestionnaires f state =
  state {questionnaires = f $ questionnaires state}

modifyQuestions
  :: (   Map (Id Question) Question
      -> Map (Id Question) Question)
  -> InMemoryState -> InMemoryState
modifyQuestions f state =
  state {questions = f $ questions state}

modifyAnswers
  :: (   Map (Id Answer) Answer
      -> Map (Id Answer) Answer)
  -> InMemoryState -> InMemoryState
modifyAnswers f state =
  state {answers = f $ answers state}

emptyState :: InMemoryState
emptyState = InMemoryState
  { questionnaires = empty
  , questions = empty
  , answers = empty
  }
