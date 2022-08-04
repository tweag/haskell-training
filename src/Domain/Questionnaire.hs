module Domain.Questionnaire where

-- text
import Data.Text

newtype Questionnaire = Questionnaire
  { title :: Text
  }
