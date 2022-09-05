{-# LANGUAGE TemplateHaskell #-}

module Api.InMemoryState where

import Domain.Answer
import Domain.Id
import Domain.Question
import Domain.Questionnaire

-- containers
import Data.Map

-- microlens-platform
import Lens.Micro.Platform

data InMemoryState = InMemoryState
  { questionnaires :: Map (Id Questionnaire) Questionnaire
  , questions      :: Map (Id Question)      Question
  , answers        :: Map (Id Answer)        Answer
  }

makeLensesFor
  [ ("questionnaires", "questionnairesL")
  , ("questions"     , "questionsL"     )
  , ("answers"       , "answersL"       )
  ]
  ''InMemoryState
