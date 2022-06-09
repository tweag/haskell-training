{-# LANGUAGE OverloadedStrings #-}

module Forms where

-- base
import Data.Bifunctor
import Text.Read (readEither)

-- hourglass
-- import Time.Types

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
  deriving Show

parseInt :: Text -> Either Text Int
parseInt = first pack . readEither . unpack

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

askMultiple :: [Question] -> IO [Answer]
askMultiple = traverse ask

-- newtype Option = Option Text

-- newtype From = From Int

-- newtype To = To Int

-- data QuestionType
--   = QuestionParagraph
--   | QuestionNumber
--   -- | QuestionMultipleChoice [Option]
--   -- | QuestionCheckboxes [Option]
--   -- | QuestionLinearScale From To
--   -- | QuestionDate
--   -- | QuestionTime

-- data Question = Question
--   { questionTitle :: Text
--   , questionType :: QuestionType
--   }

-- data Answer
--   = AnswerParagraph Text
--   | AnswerNumber Int
--   -- | AnswerMultipleChoice Option
--   -- | AnswerCheckboxes [Option]
--   -- | AnswerLinearScale Int
--   -- | AnswerDate Date
--   -- | AnswerTime TimeOfDay
--   deriving stock Show

-- ask :: Question -> IO Answer
-- ask question@(Question title questionType) = do
--   TextIO.putStrLn title
--   case questionType of
--     QuestionParagraph -> AnswerParagraph <$> TextIO.getLine
--     QuestionNumber    -> do
--       answer <- TextIO.getLine
--       case readEither (unpack answer) of
--         Left s -> do
--           putStrLn $ "wrong format: " <> s <> ". Try again"
--           ask question
--         Right numberAnswer -> pure $ AnswerNumber numberAnswer

-- askMultiple :: [Question] -> IO [Answer]
-- askMultiple = traverse ask
