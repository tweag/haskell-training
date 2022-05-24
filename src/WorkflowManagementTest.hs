{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module WorkflowManagementTest where

-- containers
import Data.Map

-- text
import Data.Text
import Data.Text.IO

class PrettyPrint a where
  prettyPrint :: a -> Text

class Parser a where
  parse :: Text -> Either Text a

newtype Transitions transition state
  = Transitions { getTransitions :: Map transition state }

instance PrettyPrint transition => PrettyPrint (Transitions transition state) where
  prettyPrint (Transitions transitions) = Data.Text.unlines . Data.Map.foldMapWithKey (\transition _ -> [prettyPrint transition]) $ transitions

data Workflow state transition = Workflow
  { currentState :: state
  , transitions  :: state -> Transitions transition state
  }

data StateTransitions state transition = StateTransitions
  { state :: state
  , availableTransitions :: Transitions transition state
  }

describe :: Workflow state transition -> (state, Transitions transition state)
describe (Workflow state f) = (state, f state)

advance :: Ord transition => Workflow state transition -> transition -> Maybe state
advance (Workflow currentState transitions) transition = Data.Map.lookup transition . getTransitions . transitions $ currentState

-- example

data ExampleState
  = A
  | B
  | C

data ExampleTransition
  = ToB
  | ToC
  deriving (Eq, Ord)

exampleWorkflow :: Workflow ExampleState ExampleTransition
exampleWorkflow = Workflow
  { currentState = A
  , transitions  = \case
      A -> Transitions $ Data.Map.fromList [(ToB, B), (ToC, C)]
      B -> Transitions $ Data.Map.fromList [(ToC, C)]
      C -> Transitions $ Data.Map.empty
  }

-- execute

execute :: (PrettyPrint state, PrettyPrint transition, Parser transition, Ord transition) => Workflow state transition -> IO ()
execute workflow = do
  Data.Text.IO.putStr . prettyPrintStateDetails $ describe workflow
  choice <- Data.Text.IO.getLine
  case parse choice of
    Left  errorMessage -> do
      Data.Text.IO.putStrLn errorMessage
      execute workflow
    Right transition -> do
      case advance workflow transition of
        Nothing    -> do
          Data.Text.IO.putStrLn $ Data.Text.unlines
            [ "Option \"" <> prettyPrint transition <> "\" not available in state \"" <> prettyPrint (currentState workflow) <> "\""
            , "Please try again."
            ]
          execute workflow
        Just state -> execute workflow {currentState = state}

prettyPrintStateDetails :: (PrettyPrint state, PrettyPrint transition) => (state, Transitions transition state) -> Text
prettyPrintStateDetails (state, transitions) = Data.Text.unlines
  [ prettyPrint state
  , prettyPrint transitions
  ]
