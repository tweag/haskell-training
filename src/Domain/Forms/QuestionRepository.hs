module Domain.Forms.QuestionRepository where

import Domain.Forms.Id
import Domain.Forms.Question
import Domain.Forms.Questionnaire

data QuestionRepository m = QuestionRepository
  { addNewQuestion                  :: Id Questionnaire -> Question -> m (Id Question)
  , selectAllQuestionnaireQuestions :: Id Questionnaire -> m [Question]
  }
