module FiniteAutomata
  ( Automaton(..)
  , StateID
  , Alphabet
  , Transition(..)
  ) where


data Automaton = Automaton { f_alphabet :: Alphabet
                           , f_states :: [StateID]
                           , f_initialState :: StateID
                           , f_acceptingStates :: [StateID]
                           , f_transitions :: [Transition]
                           } deriving (Show, Eq)

type Alphabet = [Char]

type StateID = Int

data Transition = Transition { f_fromState :: StateID
                             , f_inputLetter :: Char
                             , f_toState :: StateID
                             } deriving (Show, Eq)
