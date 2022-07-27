module Infrastructure.Serializer where

import Domain.Forms.Id
import qualified Domain.Forms.Questionnaire as Domain
import qualified Domain.Forms.Questionnaire as Questionnaire
import Infrastructure.Persistence

-- rel8
import Rel8

serializeQuesionnaire :: Id Domain.Questionnaire -> Domain.Questionnaire -> Questionnaire Result
serializeQuesionnaire questionnaireId questionnaire = Questionnaire
  { questionnaireId    = questionnaireId
  , questionnaireTitle = Questionnaire.title questionnaire
  }

deserializeQuestionnaire :: Questionnaire Result -> (Id Domain.Questionnaire, Domain.Questionnaire)
deserializeQuestionnaire questionnaire =
  ( questionnaireId questionnaire
  , Domain.Questionnaire $ questionnaireTitle questionnaire
  )
