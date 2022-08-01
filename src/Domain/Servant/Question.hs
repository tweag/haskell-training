-- {-# LANGUAGE DeriveAnyClass #-}
-- {-# LANGUAGE DeriveGeneric #-}
-- {-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}

module Domain.Servant.Question where

import Domain.Servant.Id
import Domain.Servant.Questionnaire

-- aeson
import Data.Aeson hiding (Number)
import Data.Aeson.Types hiding (Number)

-- -- base
-- import GHC.Generics

-- text
import Data.Text

data AnswerType
  = Paragraph
  | Number
  -- deriving stock Generic
  -- deriving anyclass FromJSON

instance FromJSON AnswerType where
  parseJSON :: Value -> Parser AnswerType
  parseJSON = withText "AnswerType" (\at -> case at of
    "Paragraph" -> pure Paragraph
    "Number"    -> pure Number
    _           -> fail "An AnswerSet could just be one of Paragraph and Number")

instance ToJSON AnswerType where
  toJSON :: AnswerType -> Value
  toJSON Paragraph = "Paragraph"
  toJSON Number    = "Number"

data Question = Question
  { title           :: Text
  , answerType      :: AnswerType
  , questionnaireId :: Id Questionnaire
  }
  -- deriving stock Generic
  -- deriving anyclass FromJSON

instance FromJSON Question where
  parseJSON :: Value -> Parser Question
  parseJSON = withObject "Question" (\q -> Question
    <$> q .: "title"
    <*> q .: "type"
    <*> q .: "questionnaire_id")

instance ToJSON Question where
  toJSON :: Question -> Value
  toJSON (Question title answerType questionnaireId) = object
    [ "title"            .= title
    , "type"             .= answerType
    , "questionnaire_id" .= questionnaireId
    ]