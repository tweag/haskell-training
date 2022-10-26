module Domain.SubmissionRepository where

import Domain.Answer
import Domain.Id
import Domain.Questionnaire

data SubmissionRepository m = SubmissionRepository
  { record              :: [AnswerData]     -> m (Id Submission)
  , allForQuestionnaire :: Id Questionnaire -> m [Id Submission]
  }
