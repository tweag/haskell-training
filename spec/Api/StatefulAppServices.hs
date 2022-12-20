module Api.StatefulAppServices where

import Api.AppServices

statefulAppServices :: AppServices
statefulAppServices = AppServices
  { questionnaireRepository = undefined
  , questionRepository      = undefined
  , submissionRepository    = undefined
  , answerRepository        = undefined
  }
