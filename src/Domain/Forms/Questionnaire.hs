module Domain.Forms.Questionnaire where

import Domain.Forms.Question

-- text
import Data.Text

data Questionnaire = Questionnaire
  { title :: Text
  , questions :: [Question]
  }
