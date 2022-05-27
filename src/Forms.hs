{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE QuantifiedConstraints #-}

module Forms where

import Data.Kind (Type)
import Text.Read (readEither)

--newtype Option = Option Text

newtype From = From Int

newtype To = To Int

data QuestionType
  = Paragraph
  | Number
  -- | MultipleChoice
  -- | LinearScale

data Question (questionType :: QuestionType) where
  ParagraphQuestion      ::               Question Paragraph
  NumberQuestion :: Question Number
  -- MultipleChoiceQuestion :: [Option]   -> Question MultipleChoice
  -- LinearScaleQuestion    :: From -> To -> Question LinearScale

data AnyQuestion = forall questionType . AnyQuestion (Question questionType)

-- data Form = Form
--   { formTitle :: Text
--   , questions :: [AnyQuestion]
--   }

data Answer (questionType :: QuestionType) where
  ParagraphAnswer      :: String     -> Answer Paragraph
  NumberAnswer :: Int -> Answer Number
  -- MultipleChoiceAnswer :: [Option] -> Answer MultipleChoice
  -- LinearScaleAnswer    :: Int      -> Answer LinearScale

instance Show (Answer questionType) where
  show (ParagraphAnswer s) = "ParagraphAnswer " <> s
  show (NumberAnswer i)    = "NumberAnswer " <> show i

data AnyAnswer = forall questionType . AnyAnswer (Answer questionType)

instance Show AnyAnswer where
  show (AnyAnswer answer) = show answer

ask :: Question questionType -> IO (Answer questionType)
ask ParagraphQuestion = do
  putStrLn "Please write a paragraph"
  ParagraphAnswer <$> getLine
ask NumberQuestion = do
  putStrLn "Please write a number"
  answer <- getLine
  case readEither answer of
    Left s -> do
      putStrLn $ "wrong format: " <> s <> ". Try again"
      ask NumberQuestion
    Right i -> pure $ NumberAnswer i

askMultiple :: [AnyQuestion] -> IO [AnyAnswer]
askMultiple [] = pure []
askMultiple (AnyQuestion question : tail) = do
  firstAnswer <- AnyAnswer <$> ask question
  (firstAnswer :) <$> askMultiple tail

data QuestionTypedList (a :: QuestionType -> Type) (questionsType :: [QuestionType]) where
  None :: QuestionTypedList a '[]
  AddOne :: a questionType -> QuestionTypedList a questionsType -> QuestionTypedList a (questionType ': questionsType)

instance (forall questionType . Show (a questionType)) => Show (QuestionTypedList a questionsType) where
  show None = "None"
  show (AddOne question otherQuestions) = "AddOne (" <> show question <> ") (" <> show otherQuestions <> ")"

askMultiple' :: QuestionTypedList Question questionTypes -> IO (QuestionTypedList Answer questionTypes)
askMultiple' None = pure None
askMultiple' (AddOne question otherQuestions) = AddOne <$> ask question <*> askMultiple' otherQuestions
