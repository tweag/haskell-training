{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Infrastructure.Database where

import Domain.Forms.Id
import qualified Domain.Forms.Answer as Domain
import qualified Domain.Forms.Question as Domain
import qualified Domain.Forms.Questionnaire as Domain

-- base
import GHC.Generics

-- rel8
import qualified Rel8 as Insert (Insert(returning))
import Rel8

-- text
import Data.Text

-- uuid
import Data.UUID

data Questionnaire f = Questionnaire
  { questionnaireId    :: Column f (Id Domain.Questionnaire)
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

data Question f = Question
  { questionId              :: Column f (Id Domain.Question)
  , questionQuestionnaireId :: Column f (Id Domain.Questionnaire)
  , questionTitle           :: Column f Text
  , questionType            :: Column f Domain.QuestionType
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

newtype AnswerSetId = AnswerSetId UUID
  deriving newtype (DBEq, DBType, Eq, Show)

data Answer f = Answer
  { answerId         :: Column f (Id Domain.Answer)
  , answerQuestionId :: Column f (Id Domain.Question)
  , answerSetId      :: Column f AnswerSetId
  , answerContent    :: Column f Domain.AnswerContent
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

questionnaireQuestions :: Id Domain.Questionnaire -> Query (Question Expr)
questionnaireQuestions questionnaireId = do
  questions <- each questionSchema
  Rel8.filter ((==. lit questionnaireId) . questionQuestionnaireId) questions

questionnaireAnswers :: Id Domain.Questionnaire -> Query (Answer Expr)
questionnaireAnswers questionnaireId = do
  question <- questionnaireQuestions questionnaireId
  answer <- each answerSchema
  where_ $ answerQuestionId answer ==. questionId question
  pure answer

groupedBy :: DBEq a => Query (Answer Expr) -> (Answer Expr -> Column Expr a) -> Query (Expr a, ListTable Expr (Answer Expr))
groupedBy answerQuery group = aggregate $ do
  answer <- answerQuery
  let groupedBy    = Rel8.groupBy (group answer)
  let groupAnswers = listAgg answer
  pure (groupedBy, groupAnswers)

answersBySetId :: Query (Expr AnswerSetId, ListTable Expr (Answer Expr))
answersBySetId = each answerSchema `groupedBy` answerSetId

answersByQuestion :: Query (Expr (Id Domain.Question), ListTable Expr (Answer Expr))
answersByQuestion = each answerSchema `groupedBy` answerQuestionId

questionnaireAnswersBySetId :: Id Domain.Questionnaire -> Query (Expr AnswerSetId, ListTable Expr (Answer Expr))
questionnaireAnswersBySetId questionnaireId = questionnaireAnswers questionnaireId `groupedBy` answerSetId

questionnaireAnswersByQuestion :: Id Domain.Questionnaire -> Query (Expr (Id Domain.Question), ListTable Expr (Answer Expr))
questionnaireAnswersByQuestion questionnaireId = questionnaireAnswers questionnaireId `groupedBy` answerQuestionId
