module Infrastructure.Serializer where

import Domain.Id
import qualified Domain.Questionnaire as Domain
import Infrastructure.Persistence

-- base
import Prelude hiding (id)

-- rel8
import Rel8

serializeQuestionnaire :: Identified Domain.Questionnaire -> Questionnaire Result
serializeQuestionnaire (Identified questionnaireId (Domain.Questionnaire title)) = Questionnaire
  { questionnaireId    = questionnaireId
  , questionnaireTitle = title
  }

deserializeQuestionnaire :: Questionnaire Result -> Identified Domain.Questionnaire
deserializeQuestionnaire (Questionnaire id title) = Identified
  { id = id
  , entity = Domain.Questionnaire title
  }
