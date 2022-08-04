module Infrastructure.Servant.Serializer where

import qualified Domain.Servant.Answer as Answer
import qualified Domain.Servant.Answer as Domain
import Domain.Servant.Id as Id
import qualified Domain.Servant.Question as Domain
import qualified Domain.Servant.Question as Question
import qualified Domain.Servant.Questionnaire as Domain
import qualified Domain.Servant.Questionnaire as Questionnaire
import Infrastructure.Servant.Persistence

-- rel8
import Rel8

serializeQuestionnaire :: Identified Domain.Questionnaire -> Questionnaire Result
serializeQuestionnaire (Identified questionnaireId questionnaire) = Questionnaire
  { questionnaireId    = questionnaireId
  , questionnaireTitle = Questionnaire.title questionnaire
  }

deserializeQuestionnaire :: Questionnaire Result -> Identified Domain.Questionnaire
deserializeQuestionnaire questionnaire = Identified
  (questionnaireId questionnaire)
  (Domain.Questionnaire (questionnaireTitle questionnaire))

serializeQuestion :: Identified Domain.Question -> Question Result
serializeQuestion (Identified questionId question) = Question
  { questionId              = questionId
  , questionQuestionnaireId = Question.questionnaireId question
  , questionTitle           = Question.title question
  , questionAnswerType      = Question.answerType question
  }

deserializeQuestion :: Question Result -> Identified Domain.Question
deserializeQuestion question = Identified
  { Id.id = questionId question
  , entity = Domain.Question
    { Question.title           = questionTitle question
    , Question.answerType      = questionAnswerType question
    , Question.questionnaireId = questionQuestionnaireId question
    }
  }

serializeAnswer :: Identified Domain.Answer -> Answer Result
serializeAnswer (Identified answerId answer) = Answer
  { answerId         = answerId
  , answerQuestionId = Answer.questionId answer
  , answerSetId      = Answer.setId answer
  , answerContent    = Answer.content answer
  }

deserializeAnswer :: Answer Result -> Identified Domain.Answer
deserializeAnswer answer = Identified
  { Id.id = answerId answer
  , entity = Domain.Answer
    { Answer.content    = answerContent answer
    , Answer.questionId = answerQuestionId answer
    , Answer.setId      = answerSetId answer
    }
  }
