{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

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


data Questionnaire f = Questionnaire
  { questionnaireId    :: Column f UUID
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

data Question f = Question
  { questionId              :: Column f UUID
  , questionQuestionnaireId :: Column f UUID
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

data Answer f = Answer
  { answerId         :: Column f UUID
  , answerQuestionId :: Column f UUID
  , answerSetId      :: Column f UUID
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
