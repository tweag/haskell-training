{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}

module Domain.Answer where

import Domain.Id
import qualified Domain.Question as Question (AnswerType(..))
import Domain.Question hiding (Paragraph, Number)

-- aeson
import Data.Aeson hiding (Number)

-- text
import Data.Text

data Answer = Answer
  { content    :: Content
  , setId      :: Id AnswerSet
  , questionId :: Id Question
  }

instance ToJSON Answer where
  toJSON :: Answer -> Value
  toJSON (Answer content setId questionId) = object
    [ "content"    .= content
    , "setId"      .= setId
    , "questionId" .= questionId
    ]

data Content
  = Paragraph Text
  | Number Int

instance ToJSON Content where
  toJSON :: Content -> Value
  toJSON (Paragraph t) = object
    [ "tag"   .= Question.Paragraph
    , "value" .= t
    ]
  toJSON (Number i) = object
    [ "tag"   .= Question.Number
    , "value" .= i
    ]

data AnswerSet = AnswerSet
