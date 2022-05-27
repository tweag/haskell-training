module WorkflowManagementNext where

data State
 = A
 | B
 | C
 deriving Eq

stateToChar :: State -> Char
stateToChar A = 'A'
stateToChar B = 'B'
stateToChar C = 'C'

type Transition = (State, State)

data Workflow = MkWorkflow
  { currentState :: State
  , transitions :: [Transition]
  }

w :: Workflow
w = MkWorkflow
  { currentState = A
  , transitions = [(A, B), (B, C), (B, C), (A, C)]
  }

availableTransitions :: Workflow -> [Transition]
availableTransitions (MkWorkflow currentState transitions) = filter equalsTheCurrentState transitions
  where
    equalsTheCurrentState :: Transition -> Bool
    equalsTheCurrentState (initialState, _) = initialState == currentState
