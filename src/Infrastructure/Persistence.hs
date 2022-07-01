{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Infrastructure.Persistence where

import qualified Domain.Forms as Domain

-- base
import GHC.Generics

-- rel8
import Rel8

-- text
import Data.Text

-- uuid
import Data.UUID

newtype QuestionnaireId = QuestionnaireId UUID
  deriving newtype (DBType, DBEq)

data Questionnaire f = Questionnaire
  { questionnaireId    :: Column f QuestionnaireId
  , questionnaireTitle :: Column f Text
  }
  deriving stock Generic
  deriving anyclass Rel8able

questionnaireSchema :: TableSchema (Questionnaire Name)
questionnaireSchema = TableSchema
  { name    = "questionnaire"
  , schema  = Nothing
  , columns = Questionnaire
    { questionnaireId    = "id"
    , questionnaireTitle = "title"
    }
  }

newtype QuestionId = QuestionId UUID
  deriving newtype (DBType, DBEq)

data Question f = Question
  { questionId              :: Column f QuestionId
  , questionQuestionnaireId :: Column f QuestionnaireId
  , questionTitle           :: Column f Text
  , questionType            :: Column f Domain.QuestionType
  }
  deriving stock Generic
  deriving anyclass Rel8able

questionSchema :: TableSchema (Question Name)
questionSchema = TableSchema
  { name    = "question"
  , schema  = Nothing
  , columns = Question
    { questionId              = "id"
    , questionQuestionnaireId = "questionnaire_id"
    , questionTitle           = "title"
    , questionType            = "qtype"
    }
  }

newtype AnswerId = AnswerId UUID
  deriving newtype (DBType, DBEq)

newtype AnswerSetId = AnswerSetId UUID
  deriving newtype (DBType, DBEq)

data Answer f = Answer
  { answerId         :: Column f AnswerId
  , answerQuestionId :: Column f QuestionId
  , answerSetId      :: Column f AnswerSetId
  , answerContent    :: Column f Domain.Answer
  }
  deriving stock Generic
  deriving anyclass Rel8able

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

-- QUERIES

allQuestionnaires :: Query (Questionnaire Expr)
allQuestionnaires = each questionnaireSchema

questionnaireQuestions :: QuestionnaireId -> Query (Question Expr)
questionnaireQuestions questionnaireId = do
  question <- each questionSchema
  where_ $ questionQuestionnaireId question ==. lit questionnaireId
  pure question

questionnaireAnswers :: QuestionnaireId -> Query (Answer Expr)
questionnaireAnswers questionnaireId = do
  question <- questionnaireQuestions questionnaireId
  answer <- each answerSchema
  where_ $ answerQuestionId answer ==. questionId question
  pure answer
