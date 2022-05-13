{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module GameEngine where

-- base
import Data.Bifunctor
import Data.Monoid
import Data.Void

-- containers
import Data.Map as Map

-- text
import Data.Text as Text
import Data.Text.IO as TextIO

newtype StateDescription = StateDescription Text
  deriving newtype (Semigroup, Monoid)

newtype OptionDescription = OptionDescription Text
  deriving newtype (Semigroup, Monoid)

instance PrettyPrint OptionDescription where
  prettyPrint :: OptionDescription -> Text
  prettyPrint (OptionDescription optionDescription) = optionDescription

class PrettyPrint a where
  prettyPrint :: a -> Text

-- | An `Option` describes a possible choice to perform.
--   It contains the message to be shown and the state it transitions to
data Option state = Option
  { transitionTo  :: state
  , optionDescription :: OptionDescription
  }

-- | The `Options state id` is a partial mapping from the identifiers of available options to the options themselves
newtype Options state id = Options {getOptions :: Map id (Option state)}

instance PrettyPrint id => PrettyPrint (Options state id) where
  prettyPrint :: Options state id -> Text
  prettyPrint (Options options) =
    if Map.null options
    then "No available options"
    else Text.unlines $
         [ "Available options :" ]
      <> foldMapWithKey prettyPrintOption options
      <> ["", "Please enter your choice"]
    where
      prettyPrintOption :: PrettyPrint id => id -> Option state -> [Text]
      prettyPrintOption optionId (Option _ optionDescription) = [prettyPrint optionId <> " - " <> prettyPrint optionDescription]


-- | The `StateDetails state id` datatype contains the relevant information for a single state of the game
data StateDetails state id = StateDetails
  { description :: StateDescription
  , options :: Options state id
  }

instance PrettyPrint id => PrettyPrint (StateDetails state id) where
  prettyPrint :: StateDetails state id -> Text
  prettyPrint (StateDetails (StateDescription stateDescription) options') = Text.unlines
    [ stateDescription
    , ""
    , prettyPrint options'
    ]

-- | A `Game state id` associates to every state its details
newtype Game state id = Game {stateDetails :: state -> StateDetails state id}

noStateGame :: Game Void id
noStateGame = Game absurd

singleStateGame :: StateDescription -> Map id OptionDescription -> Game () id
singleStateGame stateDescription options = Game . const $ StateDetails
  { description = stateDescription
  , options = Options $ Option () <$> options
  }

inspect :: Game state id -> state -> StateDetails state id
inspect (Game stateDetails) = stateDetails

play :: Ord id => Game state id -> state -> id -> Maybe state
play (Game stateDetails) state id' =
  let
    (StateDetails _ (Options stateOptions)) = stateDetails state
    lookupOption = Map.lookup id' stateOptions
  in
    transitionTo <$> lookupOption

class Parse a where
  parse :: Text -> Either Text a

gameLoop :: (Ord id, Parse id, PrettyPrint id, PrettyPrint state) => Game state id -> state -> IO ()
gameLoop game state = do
  TextIO.putStr . prettyPrint $ inspect game state
  playerChoice <- TextIO.getLine
  case parse playerChoice of
    Left message -> do
      TextIO.putStrLn message
      gameLoop game state
    Right optionId -> do
      case play game state optionId of
        Nothing -> do
          TextIO.putStrLn $ Text.unlines
            [ "Option " <> prettyPrint optionId <> " not available in state " <> prettyPrint state
            , "Please try again."
            ]
          gameLoop game state
        Just newState -> gameLoop game newState

-- example

newtype OptionId = Id Text
  deriving (Eq, Ord)

instance PrettyPrint OptionId where
  prettyPrint :: OptionId -> Text
  prettyPrint (Id optionId) = optionId

instance Parse OptionId where
  parse :: Text -> Either Text OptionId
  parse "" = Left $ Text.unlines
    [ "The empty string is not allowed as a choice"
    , "Please try again"
    ]
  parse t = Right (Id t)

data ExampleState
  = StateA
  | StateB
  | StateC

instance PrettyPrint ExampleState where
  prettyPrint StateA = "State A"
  prettyPrint StateB = "State B"
  prettyPrint StateC = "State C"

exampleStateDetails :: ExampleState -> StateDetails ExampleState OptionId
exampleStateDetails = \case
  StateA -> StateDetails
    { description = StateDescription "you are in state A"
    , options = Options $ Map.fromList
      [ (Id "b", Option StateB (OptionDescription "move to state B"))
      , (Id "c", Option StateC (OptionDescription "move to state C"))
      ]
    }
  StateB -> StateDetails
    { description = StateDescription "you are in state B"
    , options = Options $ Map.fromList
      [(Id "c", Option StateC (OptionDescription "move to state C"))]
    }
  StateC -> StateDetails
    { description = StateDescription "you are in state C. This is the end"
    , options = Options Map.empty
    }

exampleGame :: Game ExampleState OptionId
exampleGame = Game exampleStateDetails
