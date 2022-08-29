{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

module Infrastructure.Persistence where

import qualified Domain.Answer as Domain
import Domain.Answer (Content)
import Domain.Id
import qualified Domain.Questionnaire as Domain
import qualified Domain.Question as Domain
import Domain.Question (AnswerType)

-- base
import GHC.Generics

-- rel8
import qualified Rel8 as Insert (Insert(returning))
import Rel8

-- text
import Data.Text

data Questionnaire f = Questionnaire
  { questionnaireId    :: Column f (Id Domain.Questionnaire)
  , questionnaireTitle :: Column f Text
  }
  deriving (Generic, Rel8able)

questionnaireSchema :: TableSchema (Questionnaire Name)
questionnaireSchema = TableSchema
  { name = "questionnaire"
  , schema = Nothing
  , columns = Questionnaire
    { questionnaireId    = "id"
    , questionnaireTitle = "title"
    }
  }

data Question f = Question
  { questionId              :: Column f (Id Domain.Question)
  , questionQuestionnaireId :: Column f (Id Domain.Questionnaire)
  , questionTitle           :: Column f Text
  , questionAnswerType      :: Column f AnswerType
  }
  deriving (Generic, Rel8able)

questionSchema :: TableSchema (Question Name)
questionSchema = TableSchema
  { name    = "question"
  , schema  = Nothing
  , columns = Question
    { questionId              = "id"
    , questionQuestionnaireId = "questionnaire_id"
    , questionTitle           = "title"
    , questionAnswerType      = "answer_type"
    }
  }

data Answer f = Answer
  { answerId         :: Column f (Id Domain.Answer)
  , answerQuestionId :: Column f (Id Domain.Question)
  , answerSetId      :: Column f (Id Domain.AnswerSet)
  , answerContent    :: Column f Content
  }
  deriving (Generic, Rel8able)

answerSchema :: TableSchema (Answer Name)
answerSchema = TableSchema
  { name    = "answer"
  , schema  = Nothing
  , columns = Answer
    { answerId         = "id"
    , answerQuestionId = "question_id"
    , answerSetId      = "set_id"
    , answerContent    = "content"
    }
  }

-- SQL: SELECT * FROM questionnaire
allQuestionnaires :: Query (Questionnaire Expr)
allQuestionnaires = each questionnaireSchema

-- SELECT * FROM question
-- WHERE questionnaier_id = :questionnaire_id
questionnaireQuestions :: Id Domain.Questionnaire -> Query (Question Expr)
questionnaireQuestions questionnaireId = do
  question <- each questionSchema
  where_ $ questionQuestionnaireId question ==. lit questionnaireId
  pure question

-- SELECT * FROM answer
-- WHERE question_id = :question_id
questionAnswers :: Id Domain.Question -> Query (Answer Expr)
questionAnswers questionId = do
  answer <- each answerSchema
  where_ $ answerQuestionId answer ==. lit questionId
  pure answer

-- SELECT * FROM answer
-- WHERE set_id = :set_id
answerSetAnswers :: Id Domain.AnswerSet -> Query (Answer Expr)
answerSetAnswers setId = do
  answer <- each answerSchema
  where_ $ answerSetId answer ==. lit setId
  pure answer

questionnaireAnswerSets :: Id Domain.Questionnaire -> Query (Expr (Id Domain.AnswerSet))
questionnaireAnswerSets questionnaireId = do
  question <- questionnaireQuestions questionnaireId
  answer <- each answerSchema
  where_ $ answerQuestionId answer ==. questionId question
  distinct . pure $ answerSetId answer

add :: Rel8able f => TableSchema (f Name) -> [f Expr] -> Insert ()
add schema rows' = Insert
  { into             = schema
  , rows             = values rows'
  , onConflict       = Abort
  , Insert.returning = pure ()
  }
