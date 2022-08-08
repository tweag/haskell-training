{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}

module Domain.Questionnaire where

-- aeson
import Data.Aeson

-- text
import Data.Text

newtype Questionnaire = Questionnaire
  { title :: Text
  }

instance ToJSON Questionnaire where
  toJSON :: Questionnaire -> Value
  toJSON (Questionnaire title) = object ["title" .= title]
