{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Database where

import qualified Forms

-- base
import GHC.Generics

-- rel8
import qualified Rel8 as Insert (Insert(returning))
import Rel8

-- text
import Data.Text

-- uuid
import Data.UUID

newtype QuestionnaireId = QuestionnaireId UUID
  deriving newtype (DBEq, DBType, Eq, Show)

data Questionnaire f = Questionnaire
  { questionnaireId    :: Column f QuestionnaireId
  , questionnaireTitle :: Column f Text
  }
  deriving stock Generic
  deriving anyclass Rel8able

questionnaireSchema :: TableSchema (Questionnaire Name)
questionnaireSchema = TableSchema
  { name = "questionnaire"
  , schema = Nothing
  , columns = Questionnaire
    { questionnaireId    = "id"
    , questionnaireTitle = "title"
    }
  }

newtype QuestionId = QuestionId UUID
  deriving newtype (DBEq, DBType, Eq, Show)

data Question f = Question
  { questionId              :: Column f QuestionId
  , questionQuestionnaireId :: Column f QuestionnaireId
  , questionTitle           :: Column f Text
  , questionType            :: Column f Forms.QuestionType
  }
  deriving stock Generic
  deriving anyclass Rel8able

questionSchema :: TableSchema (Question Name)
questionSchema = TableSchema
  { name = "question"
  , schema = Nothing
  , columns = Question
    { questionId              = "id"
    , questionQuestionnaireId = "questionnaire_id"
    , questionTitle           = "title"
    , questionType            = "qtype"
    }
  }

newtype AnswerId = AnswerId UUID
  deriving newtype (DBEq, DBType, Eq, Show)

newtype AnswerSetId = AnswerSetId UUID
  deriving newtype (DBEq, DBType, Eq, Show)

data Answer f = Answer
  { answerId         :: Column f AnswerId
  , answerQuestionId :: Column f QuestionId
  , answerSetId      :: Column f AnswerSetId
  , answerContent    :: Column f Forms.Answer
  }
  deriving stock Generic
  deriving anyclass Rel8able

deriving stock instance f ~ Result => Show (Answer f)

answerSchema :: TableSchema (Answer Name)
answerSchema = TableSchema
  { name = "answer"
  , schema = Nothing
  , columns = Answer
    { answerId         = "id"
    , answerQuestionId = "question_id"
    , answerSetId      = "set_id"
    , answerContent    = "content"
    }
  }

-- QUERIES

add :: Rel8able f => TableSchema (f Name) -> [f Expr] -> Insert ()
add schema rows' = Insert
  { into             = schema
  , rows             = values rows'
  , onConflict       = Abort
  , Insert.returning = pure ()
  }

allQuestionnaires :: Query (Questionnaire Expr)
allQuestionnaires = each questionnaireSchema

questionnaireQuestions :: QuestionnaireId -> Query (Question Expr)
questionnaireQuestions questionnaireId = do
  questions <- each questionSchema
  Rel8.filter ((==. lit questionnaireId) . questionQuestionnaireId) questions

questionnaireAnswers :: QuestionnaireId -> Query (Answer Expr)
questionnaireAnswers questionnaireId = do
  question <- questionnaireQuestions questionnaireId
  answer <- each answerSchema
  where_ $ answerQuestionId answer ==. questionId question
  pure answer

-- questionnaireAnswersByQuestion :: QuestionnaireId -> Query (Answer Expr)
-- questionnaireAnswersByQuestion questionnaireId = do
--   answer <- questionnaireAnswers questionnaireId
--   aggregate $ Rel8.groupBy _

answersBySetId :: Query (Expr AnswerSetId, ListTable Expr (Answer Expr))
answersBySetId = answersGroupedBy answerSetId

answersByQuestion :: Query (Expr QuestionId, ListTable Expr (Answer Expr))
answersByQuestion = answersGroupedBy answerQuestionId

answersGroupedBy :: DBEq a => (Answer Expr -> Column Expr a) -> Query (Expr a, ListTable Expr (Answer Expr))
answersGroupedBy group = aggregate $ do
  answer <- each answerSchema
  let groupedBy    = Rel8.groupBy (group answer)
  let groupAnswers = listAgg answer
  pure (groupedBy, groupAnswers)
