{-# LANGUAGE OverloadedStrings #-}

module Api.FormsSpec where

import Api.Forms
import Domain.Answer as Answer
import Domain.Question as Question
import Domain.Questionnaire

-- base
import Data.Either

-- hspec
import Test.Hspec

-- servant-server
import Servant

spec :: Spec
spec =
  describe "Forms API" $ do
    let forms = formsServer undefined

    describe "register answers endpoint" $ do
      it "fails if we do not register an answer \
          \for every question of the questionnaire" $ do
        submissionId <- runHandler $ do
          questionnaireId <- createNewQuestionnaire forms $ Questionnaire "title"
          questionId1 <- addNewQuestion forms $ Question "question1" Question.Paragraph questionnaireId
          _           <- addNewQuestion forms $ Question "question2" Question.Number    questionnaireId
          recordSubmission forms
            [ AnswerData (Answer.Paragraph "answer") questionId1
            ]
        submissionId `shouldSatisfy` isLeft
