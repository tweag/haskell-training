module WorkflowManagementGraph where

-- containers
import Data.Map

-- graph LR
--     A --> B
--     B --> C
--     B --> C
--     A --> C

data State
  = A
  | B
  | C

data Workflow = Workflow
  { currentState :: State
  , transitions :: [(State, State)]
  }

w :: Workflow
w = Workflow A [(A, B), (B, C), (B, C), (A, C)]

-- to know which are the currently available transitions, we need every time to scan all transitions

-- so we group transitions which have the same initial state

data Workflow' = Workflow'
  { currentState' :: State
  , transitions'  :: [(State, [State])]
  }

w' :: Workflow'
w' = Workflow' A [(A, [B, C]), (B, [C, C])]

-- this is allowing an initial state to appear several times, e.g. `transitions = [(A, [A]), (A, [B])]`

-- to disallow duplicates, we use a function

data Workflow'' = Workflow''
  { currentState'' :: State
  , transitions''  :: State -> [State]
  }

w'' :: Workflow''
w'' = Workflow'' A (\initialState -> case initialState of
  A -> [B, C]
  B -> [C, C]
  C -> [])

-- to select the transition, we need to use the `index` of the new state in the list
-- moreover we can not distinguish multiple transitions between the same states

-- therefore we introduce our own indices

data Transition
  = A2B
  | A2C
  | B2C1
  | B2C2
  deriving (Eq, Ord)

data Workflow''' = Workflow'''
  { currentState''' :: State
  , transitions'''  :: State -> Map Transition State
  }

w''' :: Workflow'''
w''' = Workflow''' A (\initialState -> case initialState of
  A -> fromList [(A2B, B), (A2C, C)]
  B -> fromList [(B2C1, C), (B2C2, C)]
  C -> empty)
