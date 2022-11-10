module Infrastructure.Serializer where

import qualified Domain.Answer as Domain
import Domain.Id
import qualified Domain.Question as Domain
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

serializeQuestion :: Identified Domain.Question -> Question Result
serializeQuestion (Identified questionId (Domain.Question title answerType questionnaireId)) = Question
  { questionId = questionId
  , questionQuestionnaireId = questionnaireId
  , questionTitle = title
  , questionAnswerType = answerType
  }

deserializeQuestion :: Question Result -> Identified Domain.Question
deserializeQuestion (Question id questionnaireId title answerType) = Identified
  { id = id
  , entity = Domain.Question title answerType questionnaireId
  }

serializeAnswer :: Id Domain.Submission -> Identified Domain.AnswerData -> Answer Result
serializeAnswer setId (Identified answerId (Domain.AnswerData content questionId)) = Answer
  { answerId = Domain.answerDataIdIsAnswerId answerId
  , answerQuestionId = questionId
  , submissionId = setId
  , answerContent = content
  }

deserializeAnswer :: Answer Result -> Identified Domain.Answer
deserializeAnswer (Answer id questionId answerSetId content) = Identified
  { id = id
  , entity = Domain.Answer content questionId answerSetId
  }