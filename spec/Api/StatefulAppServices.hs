module Api.StatefulAppServices where

import Api.AppServices

statefulAppServices :: AppServices
statefulAppServices = AppServices
  { questionnaireRepository = _
  , questionRepository      = _
  , answerSetRepository     = _
  , answerRepository        = _
  }
