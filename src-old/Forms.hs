{-# LANGUAGE OverloadedStrings #-}

module Forms where

-- base
import Data.Bifunctor
import Text.Read (readEither)

-- text
import Data.Text
import qualified Data.Text.IO as Text

data Question = MkQuestion
  { title      :: Text
  , answerType :: AnswerType
  }

data AnswerType
  = Paragraph
  | Number

whatIsYourName :: Question
whatIsYourName = MkQuestion
  { title = "What is your name?"
  , answerType = Paragraph
  }

howOldAreYou :: Question
howOldAreYou = MkQuestion
  { title = "How old are you?"
  , answerType = Number
  }

data Answer
  = ParagraphAnswer Text
  | NumberAnswer Int
  deriving Show

ask :: Question -> IO Answer
ask question = do
  Text.putStrLn (title question)
  answer <- Text.getLine
  case answerType question of
    Paragraph -> pure (ParagraphAnswer answer)
    Number    ->
      case parseInt answer of
        Left errorMessage -> do
          Text.putStrLn ("invalid integer: " <> errorMessage <> ". Try again")
          ask question
        Right intAnswer   -> pure (NumberAnswer intAnswer)

parseInt :: Text -> Either Text Int
parseInt = first pack . readEither . unpack

askMultiple :: [Question] -> IO [Answer]
askMultiple []                         = pure []
askMultiple (question : nextQuestions) = do
  answer <- ask question
  nextAnswers <- askMultiple nextQuestions
  pure (answer : nextAnswers)
