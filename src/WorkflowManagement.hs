{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module WorkflowManagement where

-- containers
import Data.Map

-- either
import Data.Either.Combinators

-- text
import Data.Text
import Data.Text.IO

class PrettyPrint a where
  prettyPrint :: a -> Text

class Parse a where
  parse :: Text -> Either Text a

newtype Transitions transition state
  = Transitions {getTransitions :: Map transition state}

instance PrettyPrint transition => PrettyPrint (Transitions transition state) where
  prettyPrint (Transitions transitions) = Data.Text.unlines . Data.Map.foldMapWithKey
      (\transition _ -> [prettyPrint transition]) $ transitions

data StateTransitions state transition = StateTransitions
  { state :: state
  , availableTransitions :: Transitions transition state
  }

instance (PrettyPrint state, PrettyPrint transition) => PrettyPrint (StateTransitions state transition) where
  prettyPrint (StateTransitions state transitions) = Data.Text.unlines
    [ prettyPrint state
    , prettyPrint transitions
    ]

data Workflow state transition = Workflow
  { currentState :: state
  , transitions  :: state -> Transitions transition state
  }

describe :: Workflow state transition -> StateTransitions state transition
describe (Workflow currentState transitions) = StateTransitions currentState (transitions currentState)

advance :: Ord transition => Workflow state transition -> transition -> Maybe state
advance (Workflow currentState transitions) transition
  = Data.Map.lookup transition . getTransitions . transitions $ currentState

-- example

data ExampleState
  = A
  | B
  | C

instance PrettyPrint ExampleState where
  prettyPrint A = "You are in state A"
  prettyPrint B = "You are in state B"
  prettyPrint C = "You are in state C. No more options available"

data ExampleTransition
  = ToB
  | ToC
  deriving (Eq, Ord)

instance PrettyPrint ExampleTransition where
  prettyPrint ToB = "b - move to state B"
  prettyPrint ToC = "c - move to state C"

instance Parse ExampleTransition where
  parse "b" = Right ToB
  parse "c" = Right ToC
  parse a   = Left $ "unable to parse \"" <> a <> "\" into an ExampleTransition"

exampleWorkflow :: Workflow ExampleState ExampleTransition
exampleWorkflow = Workflow
  { currentState = A
  , transitions  = \case
      A -> Transitions $ Data.Map.fromList [(ToB, B), (ToC, C)]
      B -> Transitions $ Data.Map.fromList [(ToC, C)]
      C -> Transitions $ Data.Map.empty
  }

-- execute

execute :: (PrettyPrint state, PrettyPrint transition, Parse transition, Ord transition) => Workflow state transition -> IO ()
execute workflow = do
  Data.Text.IO.putStr . prettyPrint $ describe workflow
  choice <- Data.Text.IO.getLine
  let unavailableOption transition = Data.Text.unlines
        [ "Option \"" <> prettyPrint transition <> "\" not available in state \"" <> prettyPrint (currentState workflow) <> "\""
        , "Please try again."
        ]
  case parse choice >>= maybeToRight <$> unavailableOption <*> advance workflow of
    Left errorMessage -> do
      Data.Text.IO.putStrLn errorMessage
      execute workflow
    Right state -> execute workflow {currentState = state}
