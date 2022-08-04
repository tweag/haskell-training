{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}

module Domain.Forms where

-- base
import Data.Bifunctor
import Text.Read (readEither)

-- rel8
import Rel8

-- text
import Data.Text
import qualified Data.Text.IO as Text

data Question = MkQuestion
  { questionTitle :: Text
  , questionType  :: QuestionType
  }

data QuestionType
  = Paragraph
  | Number
  deriving stock (Read, Show)
  deriving DBType via ReadShow QuestionType

whatIsYourName :: Question
whatIsYourName = MkQuestion
  { questionTitle = "What is your name?"
  , questionType = Paragraph
  }

howOldAreYou :: Question
howOldAreYou = MkQuestion
  { questionTitle = "How old are you?"
  , questionType = Number
  }

data Answer
  = ParagraphAnswer Text
  | NumberAnswer Int
  deriving stock (Read, Show)
  deriving DBType via ReadShow Answer

ask :: Question -> IO Answer
ask question = do
  Text.putStrLn (questionTitle question)
  answer <- Text.getLine
  case questionType question of
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
askMultiple = traverse ask
