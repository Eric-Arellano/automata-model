module AutomatonTypes
  ( InputLanguage
  , State
  , TransitionFunction(..)
  , ProgramData(..)
  ) where

type InputLanguage = [Char]
type State = Int
data TransitionFunction = MakeTransitionFunction { from :: State
                                                 , transition :: Char
                                                 , to :: State
                                                 } deriving (Show)

data ProgramData = MakeProgramData { inputLanguage :: InputLanguage,
                                   transitionFunctions :: [TransitionFunction],
                                   startingState :: State,
                                   acceptingStates :: [State] } deriving (Show)