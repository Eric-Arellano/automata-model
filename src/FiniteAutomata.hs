module FiniteAutomata
  ( Automaton(..)
  , StateID
  , Alphabet
  , Transition(..)
  , DFA
  , toDFA
  , isNFA
  , isDFA
  , complement
  ) where


import qualified Data.List      as List
import qualified Data.Maybe     as Maybe
import qualified Control.Monad  as Monad
import qualified Data.Monoid    as Monoid
import qualified Data.Ord       as Ord

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

type DFA = Automaton


-- -------------------------------------------------------------------
-- Check automaton  type
-- -------------------------------------------------------------------

isDFA :: Automaton -> Bool
isDFA automaton = all everyInputDefined (f_states automaton)
  where
    everyInputDefined :: StateID -> Bool
    everyInputDefined state = all (hasLetter state) (f_alphabet automaton)
    hasLetter :: StateID -> Char -> Bool
    hasLetter state letter = letter `List.elem` (getTransitionLetters state)
    getTransitionLetters :: StateID -> [Char]
    getTransitionLetters state = map f_inputLetter . filter (\t -> f_fromState t == state) $ f_transitions automaton


isNFA :: Automaton -> Bool
isNFA = not . isDFA


-- -------------------------------------------------------------------
-- Convert NFA to DFA
-- -------------------------------------------------------------------

type ConvID = [StateID]

data ConvAutomaton = ConvAutomaton { f_convAlphabet :: Alphabet
                                   , f_convStates :: [ConvID]
                                   , f_convInitialState :: ConvID
                                   , f_convAcceptingStates :: [ConvID]
                                   , f_convTransitions :: [ConvTransition]
                                   } deriving (Eq, Show)

data ConvTransition = ConvTransition { f_convFromState :: ConvID
                                     , f_convInputLetter :: Char
                                     , f_convToState :: ConvID
                                     } deriving (Eq, Show)

toDFA :: Automaton -> DFA
toDFA automaton
  | isDFA automaton = automaton
  | otherwise       = convertBack
                    . removeUselessStates
                    . initConvAutomaton
                    $ automaton


initConvAutomaton :: Automaton -> ConvAutomaton
initConvAutomaton automaton = ConvAutomaton { f_convAlphabet = f_alphabet automaton
                                            , f_convStates = convertedStates
                                            , f_convInitialState = convertInitialState automaton
                                            , f_convAcceptingStates = convertAcceptingStates automaton convertedStates
                                            , f_convTransitions = convertTransitionFunctions automaton convertedStates }
  where
    convertedStates :: [ConvID]
    convertedStates = powerset (f_states automaton)
    powerset :: [a] -> [[a]]
    powerset = Monad.filterM (const [True, False])


convertInitialState :: Automaton -> ConvID
convertInitialState automaton = [f_initialState automaton]


convertAcceptingStates :: Automaton -> [ConvID] -> [ConvID]
convertAcceptingStates automaton convStateIDs = filter containsAcceptingID convStateIDs
  where
    containsAcceptingID :: ConvID -> Bool
    containsAcceptingID convStateID = not . null $ convStateID `List.intersect` (f_acceptingStates automaton)


convertTransitionFunctions :: Automaton -> [ConvID] -> [ConvTransition]
convertTransitionFunctions automaton convStateIDs = foldl (++) [] . map findTransitions $ convStateIDs
  where
    findTransitions :: ConvID -> [ConvTransition]
    findTransitions convID = map (createTransition convID) (f_alphabet automaton)
    createTransition :: ConvID -> Char -> ConvTransition
    createTransition convID char = ConvTransition { f_convFromState = convID
                                                  , f_convInputLetter = char
                                                  , f_convToState = getAllToStates convID char }  -- [] if nothing found
    getAllToStates :: ConvID -> Char -> [StateID]
    getAllToStates convID char = foldl (++) [] . map (getToStates char) $ convID
    getToStates :: Char -> StateID -> [StateID]
    getToStates char stateID = map f_toState
                             . filter (\t ->  (f_inputLetter t) == char && (f_fromState t) == stateID)
                             $ f_transitions automaton


removeUselessStates :: ConvAutomaton -> ConvAutomaton
removeUselessStates convAutomaton = convAutomaton { f_convStates = validStates
                                                  , f_convAcceptingStates = reducedAccepting
                                                  , f_convTransitions = reducedTransitions }
  where
    validStates :: [ConvID]
    validStates = filter (\s -> s == (f_convInitialState convAutomaton) || isReachable s) (f_convStates convAutomaton)
    reducedTransitions :: [ConvTransition]
    reducedTransitions = filter (\t -> (f_convFromState t) `List.elem` validStates) (f_convTransitions convAutomaton)
    reducedAccepting :: [ConvID]
    reducedAccepting = filter (\a -> a `List.elem` validStates) (f_convAcceptingStates convAutomaton)
    isReachable :: ConvID -> Bool  -- TODO: should this check if reachable from initial? Only checks reachable from another state.
    isReachable convStateID = convStateID `elem` allToStates
    allToStates :: [ConvID]
    allToStates = map f_convToState (f_convTransitions convAutomaton)


convertBack :: ConvAutomaton -> Automaton
convertBack convAutomaton = Automaton { f_alphabet = f_convAlphabet convAutomaton
                                      , f_states = map convertID (f_convStates convAutomaton)
                                      , f_initialState = convertID (f_convInitialState convAutomaton)
                                      , f_acceptingStates = map convertID (f_convAcceptingStates convAutomaton)
                                      , f_transitions = map convertTransition (f_convTransitions convAutomaton) }
  where
    convertID :: ConvID -> StateID
    convertID convID = Maybe.fromMaybe (-1) (lookup convID incrementedStateIDs)
    convertTransition :: ConvTransition -> Transition
    convertTransition convTransition = Transition { f_fromState = convertID (f_convFromState convTransition)
                                                  , f_inputLetter = f_convInputLetter convTransition
                                                  , f_toState = convertID (f_convToState convTransition) }
    incrementedStateIDs :: [(ConvID, StateID)]
    incrementedStateIDs = snd $ List.mapAccumL(\index conv -> (index + 1, (conv, index))) 0 allConvIDs
    allConvIDs :: [ConvID]
    allConvIDs = List.sortBy (Monoid.mconcat [Ord.comparing length, compare]) (f_convStates convAutomaton)


-- -------------------------------------------------------------------
-- Modify DFA
-- -------------------------------------------------------------------

complement :: DFA -> DFA
complement dfa = dfa { f_acceptingStates = invertedAcceptStates }
  where
    invertedAcceptStates = filter (\s -> not (s `List.elem` originalAcceptStates)) (f_states dfa)
    originalAcceptStates = f_acceptingStates dfa
