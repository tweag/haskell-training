-- {-# LANGUAGE DeriveAnyClass #-}
-- {-# LANGUAGE DeriveGeneric #-}
-- {-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}


module Domain.Servant.Questionnaire where

-- aeson
import Data.Aeson
import Data.Aeson.Types

-- -- base
-- import GHC.Generics

-- text
import Data.Text

newtype Questionnaire = Questionnaire
  { title :: Text
  }
  -- deriving stock Generic
  -- deriving anyclass FromJSON

instance FromJSON Questionnaire where
  parseJSON :: Value -> Parser Questionnaire
  parseJSON (Object question) = Questionnaire <$> question .: "title"
  parseJSON v                 = typeMismatch "Object" v

-- instance FromJSON Questionnaire where
--   parseJSON :: Value -> Parser Questionnaire
--   parseJSON = withObject "Questionnaire" (\q -> Questionnaire <$> q .: "title")

instance ToJSON Questionnaire where
  toJSON :: Questionnaire -> Value
  toJSON (Questionnaire title) = object
    [ "title" .= title ]
