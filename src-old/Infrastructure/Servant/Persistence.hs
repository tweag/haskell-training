{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Infrastructure.Servant.Persistence where

import qualified Domain.Servant.Answer as Answer
import qualified Domain.Servant.Answer as Domain
import Domain.Servant.Id
import qualified Domain.Servant.Question as Domain
import qualified Domain.Servant.Questionnaire as Domain

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
  , questionAnswerType      :: Column f Domain.AnswerType
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
    , questionAnswerType      = "qtype"
    }
  }

data Answer f = Answer
  { answerId         :: Column f (Id Domain.Answer)
  , answerQuestionId :: Column f (Id Domain.Question)
  , answerSetId      :: Column f (Id Domain.AnswerSet)
  , answerContent    :: Column f Answer.Content
  }
  deriving (Generic, Rel8able)

deriving stock instance f ~ Result => Show (Answer f)

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

add :: Rel8able f => TableSchema (f Name) -> [f Expr] -> Insert ()
add schema rows' = Insert
  { into             = schema
  , rows             = values rows'
  , onConflict       = Abort
  , Insert.returning = pure ()
  }

-- SQL: SELECT * FROM questionnaire
allQuestionnaires :: Query (Questionnaire Expr)
allQuestionnaires = each questionnaireSchema

questionnaireQuestions :: Id Domain.Questionnaire -> Query (Question Expr)
questionnaireQuestions questionnaireId = do
  question <- each questionSchema
  where_ $ questionQuestionnaireId question ==. lit questionnaireId
  pure question

questionnaireAnswerSets :: Id Domain.Questionnaire -> Query (Expr (Id Domain.AnswerSet))
questionnaireAnswerSets questionnaireId = do
  answer <- distinctOn answerSetId $ each answerSchema
  question <- questionnaireQuestions questionnaireId
  where_ $ answerQuestionId answer ==. questionId question
  pure $ answerSetId answer

answerSetAnswers :: Id Domain.AnswerSet -> Query (Answer Expr)
answerSetAnswers answerSetId' = do
  answer <- each answerSchema
  where_ $ answerSetId answer ==. lit answerSetId'
  pure answer

questionAnswers :: Id Domain.Question -> Query (Answer Expr)
questionAnswers questionId = do
  answer <- each answerSchema
  where_ $ answerQuestionId answer ==. lit questionId
  pure answer

-- questionnaireAnswers :: Id Domain.Questionnaire -> Query (Answer Expr)
-- questionnaireAnswers questionnaireId = do
--   question <- questionnaireQuestions questionnaireId
--   answer <- each answerSchema
--   where_ $ answerQuestionId answer ==. questionId question
--   pure answer

-- groupedBy :: DBEq a => Query (Answer Expr) -> (Answer Expr -> Column Expr a) -> Query (Expr a, ListTable Expr (Answer Expr))
-- groupedBy answerQuery group = aggregate $ do
--   answer <- answerQuery
--   let groupedBy    = Rel8.groupBy (group answer)
--   let groupAnswers = listAgg answer
--   pure (groupedBy, groupAnswers)
