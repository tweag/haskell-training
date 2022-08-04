-- {-# LANGUAGE DeriveAnyClass #-}
-- {-# LANGUAGE DeriveGeneric #-}
-- {-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}

module Domain.Servant.Answer where

import Domain.Servant.Id
import Domain.Servant.Question


-- aeson
import Data.Aeson hiding (Number)
import Data.Aeson.Types hiding (Number)

-- base
-- import GHC.Generics

-- rel8
import Rel8

-- text
import Data.Text

data Content
  = ParagraphAnswer Text
  | NumberAnswer Int
  deriving stock (Read, Show)
  deriving DBType via ReadShow Content
  -- deriving stock Generic
  -- deriving anyclass FromJSON

instance FromJSON Content where
  parseJSON :: Value -> Parser Content
  parseJSON = withObject "Content" (\c -> do
    contentType <- c .: "type"
    case contentType of
      Paragraph -> ParagraphAnswer <$> c .: "value"
      Number    -> NumberAnswer    <$> c .: "value")

instance ToJSON Content where
  toJSON :: Content -> Value
  toJSON (ParagraphAnswer t) = object
    [ "type"  .= toJSON Paragraph
    , "value" .= toJSON t
    ]
  toJSON (NumberAnswer n) = object
    [ "type"  .= toJSON Number
    , "value" .= toJSON n
    ]

data AnswerSet = AnswerSet

data Answer = Answer
  { content    :: Content
  , questionId :: Id Question
  , setId      :: Id AnswerSet
  }
  -- deriving stock Generic
  -- deriving anyclass FromJSON

instance FromJSON Answer where
  parseJSON :: Value -> Parser Answer
  parseJSON = withObject "Answer" (\a -> Answer
    <$> a .: "content"
    <*> a .: "question_id"
    <*> a .: "set_id")

instance ToJSON Answer where
  toJSON :: Answer -> Value
  toJSON (Answer content questionId setId) = object
    [ "content"     .= content
    , "question_id" .= questionId
    , "set_id"      .= setId
    ]
