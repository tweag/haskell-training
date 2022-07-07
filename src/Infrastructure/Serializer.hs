module Infrastructure.Serializer where

import Domain.Forms.Id
import qualified Domain.Forms.Question as Domain
import qualified Domain.Forms.Question as Question
import qualified Domain.Forms.Questionnaire as Domain
import qualified Domain.Forms.Questionnaire as Questionnaire
import Infrastructure.Database

-- rel8
import Rel8

-- Questionnaire

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

-- Question

serializeQuestion :: Id Domain.Question -> Id Domain.Questionnaire -> Domain.Question -> Question Result
serializeQuestion questionId questionnaireId question = Question
  { questionId              = questionId
  , questionQuestionnaireId = questionnaireId
  , questionTitle           = Question.title question
  , questionType            = Question.qtype question
  }

deserializeQuestion :: Question Result -> (Id Domain.Question, Id Domain.Questionnaire, Domain.Question)
deserializeQuestion question =
  ( questionId question
  , questionQuestionnaireId question
  , Domain.Question
      (questionTitle question)
      (questionType question)
  )
