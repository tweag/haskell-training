module Forms where

-- base
import Text.Read (readEither)

data Question = MkQuestion
  { title      :: String
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
  = ParagraphAnswer String
  | NumberAnswer Int
  deriving Show

ask :: Question -> IO Answer
ask question = do
  putStrLn (title question)
  answer <- getLine
  case answerType question of
    Paragraph -> pure (ParagraphAnswer answer)
    Number    ->
      case parseInt answer of
        Left errorMessage -> do
          putStrLn (  "invalid integer: "
                   <> errorMessage
                   <> ". Try again"
                   )
          ask question
        Right intAnswer   -> pure (NumberAnswer intAnswer)

parseInt :: String -> Either String Int
parseInt = readEither
