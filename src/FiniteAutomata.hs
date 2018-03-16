module FiniteAutomata
  ( Automaton(..)
  , State(..)
  , Transition(..)
  , output
  , toDFA
  , isDFA
  , complement
  ) where


import qualified Data.List as List


data Automaton = Automaton { alphabet :: Alphabet
                           , states :: [State]
                           } deriving (Show, Eq)

type Alphabet = [Char]

data State = State { number :: Int
                   , isInitial :: Bool
                   , isAccepting :: Bool
                   , transitions :: [Transition]
                   } deriving (Show, Eq)

data Transition = Transition { inputLetter :: Char
                             , toState :: State
                             } deriving (Show, Eq)

type DFA = Automaton


toDFA :: Automaton -> DFA
toDFA = undefined


isDFA :: Automaton -> Bool
isDFA dfa = undefined
  where
    everyInputDefined state = undefined


complement :: DFA -> DFA
complement dfa = dfa { states = map invertAccept (states dfa) }
  where
    invertAccept state = state { isAccepting = not (isAccepting state)}


intersection :: DFA -> DFA -> DFA
intersection = undefined


output :: Automaton -> String
output automaton = removeSlash ("Automaton " ++ outputAlphabet ++ outputStates)
  where
    removeSlash = filter (\x -> not (x `List.elem` "\"\\"))
    outputAlphabet = show ("Alphabet = " ++ alphabet automaton) ++ ", "
    outputStates = (show "states = [") ++ (foldl (++) "" condensedStates) ++ (show "]")
    condensedStates = map condense (states automaton)
    condense state = show "State {id = "
                     ++ show (number state)
                     ++ show ", isInitial = "
                     ++ show (isInitial state)
                     ++ show ", isAccepting = "
                     ++ show (isAccepting state)
                     ++ show ", transitions = ["
                     ++ outputTransitions (transitions state)
                     ++ show "]}"
    outputTransitions trans = foldl (++) "" (map condenseTransition trans)
    condenseTransition transition = show "Transition = {inputLetter = "
                                    ++ show (inputLetter transition)
                                    ++ show ", toState = "
                                    ++ show (number (toState transition))
                                    ++ show "} "