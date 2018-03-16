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
import qualified Data.Maybe as Maybe

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
isDFA automaton = all everyInputDefined (states automaton)
  where
    everyInputDefined :: State -> Bool
    everyInputDefined state = all (hasLetter state) (alphabet automaton)
    hasLetter :: State -> Char -> Bool
    hasLetter state letter = letter `List.elem` (transitionLetters state)
    transitionLetters :: State -> [Char]
    transitionLetters state = map inputLetter (transitions state)


complement :: DFA -> DFA
complement dfa = dfa { states = map invertAccept (states dfa) }
  where
    invertAccept :: State -> State
    invertAccept state = state { isAccepting = not (isAccepting state)}


intersection :: DFA -> DFA -> DFA
intersection = undefined


output :: Automaton -> String
output automaton = removeSlash ("Automaton " ++ outputAlphabet ++ outputStates)
  where
    removeSlash :: String -> String
    removeSlash = filter (\x -> not (x `List.elem` "\"\\"))
    outputAlphabet :: String
    outputAlphabet = show ("Alphabet = " ++ alphabet automaton) ++ ", "
    outputStates :: String
    outputStates = (show "states = [") ++ (foldl (++) "" condensedStates) ++ (show "]")
    condensedStates :: [String]
    condensedStates = map condense (states automaton)
    condense :: State -> String
    condense state = show "State {id = "
                     ++ show (number state)
                     ++ show ", isInitial = "
                     ++ show (isInitial state)
                     ++ show ", isAccepting = "
                     ++ show (isAccepting state)
                     ++ show ", transitions = ["
                     ++ outputTransitions (transitions state)
                     ++ show "]}"
    outputTransitions :: [Transition] -> String
    outputTransitions trans = foldl (++) "" (map condenseTransition trans)
    condenseTransition :: Transition -> String
    condenseTransition transition = show "Transition = {inputLetter = "
                                    ++ show (inputLetter transition)
                                    ++ show ", toState = "
                                    ++ show (number (toState transition))
                                    ++ show "} "