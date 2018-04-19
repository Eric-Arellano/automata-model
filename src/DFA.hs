module DFA
  ( DFA
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

import qualified FiniteAutomata as FA
import qualified ShortestString

type DFA = FA.Automaton


-- -------------------------------------------------------------------
-- Check automaton  type
-- -------------------------------------------------------------------

isDFA :: FA.Automaton -> Bool
isDFA automaton = all everyInputDefined (FA.f_states automaton)
  where
    everyInputDefined :: FA.StateID -> Bool
    everyInputDefined state = all (hasLetter state) (FA.f_alphabet automaton)
    hasLetter :: FA.StateID -> Char -> Bool
    hasLetter state letter = letter `List.elem` (getTransitionLetters state)
    getTransitionLetters :: FA.StateID -> [Char]
    getTransitionLetters state = map FA.f_inputLetter . filter (\t -> FA.f_fromState t == state) $ FA.f_transitions automaton


isNFA :: FA.Automaton -> Bool
isNFA = not . isDFA


-- -------------------------------------------------------------------
-- Convert NFA to DFA
-- -------------------------------------------------------------------

type ConvID = [FA.StateID]

data ConvAutomaton = ConvAutomaton { f_convAlphabet :: FA.Alphabet
                                   , f_convStates :: [ConvID]
                                   , f_convInitialState :: ConvID
                                   , f_convAcceptingStates :: [ConvID]
                                   , f_convTransitions :: [ConvTransition]
                                   } deriving (Eq, Show)

data ConvTransition = ConvTransition { f_convFromState :: ConvID
                                     , f_convInputLetter :: Char
                                     , f_convToState :: ConvID
                                     } deriving (Eq, Show)

toDFA :: FA.Automaton -> DFA
toDFA automaton
  | isDFA automaton = automaton
  | otherwise       = removeUselessStates
                    . convertBack
                    . initConvAutomaton
                    $ automaton


initConvAutomaton :: FA.Automaton -> ConvAutomaton
initConvAutomaton automaton = ConvAutomaton { f_convAlphabet = FA.f_alphabet automaton
                                            , f_convStates = convertedStates
                                            , f_convInitialState = convertInitialState automaton
                                            , f_convAcceptingStates = convertAcceptingStates automaton convertedStates
                                            , f_convTransitions = convertTransitionFunctions automaton convertedStates }
  where
    convertedStates :: [ConvID]
    convertedStates = powerset (FA.f_states automaton)
    powerset :: [a] -> [[a]]
    powerset = Monad.filterM (const [True, False])


convertInitialState :: FA.Automaton -> ConvID
convertInitialState automaton = [FA.f_initialState automaton]


convertAcceptingStates :: FA.Automaton -> [ConvID] -> [ConvID]
convertAcceptingStates automaton convStateIDs = filter containsAcceptingID convStateIDs
  where
    containsAcceptingID :: ConvID -> Bool
    containsAcceptingID convStateID = not . null $ convStateID `List.intersect` (FA.f_acceptingStates automaton)


convertTransitionFunctions :: FA.Automaton -> [ConvID] -> [ConvTransition]
convertTransitionFunctions automaton convStateIDs = foldl (++) [] . map findTransitions $ convStateIDs
  where
    findTransitions :: ConvID -> [ConvTransition]
    findTransitions convID = map (createTransition convID) (FA.f_alphabet automaton)
    createTransition :: ConvID -> Char -> ConvTransition
    createTransition convID char = ConvTransition { f_convFromState = convID
                                                  , f_convInputLetter = char
                                                  , f_convToState = getAllToStates convID char }  -- [] if nothing found
    getAllToStates :: ConvID -> Char -> [FA.StateID]
    getAllToStates convID char = List.nub . foldl (++) [] . map (getToStates char) $ convID
    getToStates :: Char -> FA.StateID -> [FA.StateID]
    getToStates char stateID = map FA.f_toState
                             . filter (\t ->  (FA.f_inputLetter t) == char && (FA.f_fromState t) == stateID)
                             $ FA.f_transitions automaton


convertBack :: ConvAutomaton -> FA.Automaton
convertBack convAutomaton = FA.Automaton { FA.f_alphabet = f_convAlphabet convAutomaton
                                         , FA.f_states = map convertID (f_convStates convAutomaton)
                                         , FA.f_initialState = convertID (f_convInitialState convAutomaton)
                                         , FA.f_acceptingStates = map convertID (f_convAcceptingStates convAutomaton)
                                         , FA.f_transitions = map convertTransition (f_convTransitions convAutomaton) }
  where
    convertID :: ConvID -> FA.StateID
    convertID convID = Maybe.fromMaybe (-1) (lookup convID incrementedStateIDs)
    convertTransition :: ConvTransition -> FA.Transition
    convertTransition convTransition = FA.Transition { FA.f_fromState = convertID (f_convFromState convTransition)
                                                     , FA.f_inputLetter = f_convInputLetter convTransition
                                                     , FA.f_toState = convertID (f_convToState convTransition) }
    incrementedStateIDs :: [(ConvID, FA.StateID)]
    incrementedStateIDs = snd $ List.mapAccumL(\index conv -> (index + 1, (conv, index))) 0 allConvIDs
    allConvIDs :: [ConvID]
    allConvIDs = List.sortBy (Monoid.mconcat [Ord.comparing length, compare]) (f_convStates convAutomaton)


removeUselessStates :: FA.Automaton -> FA.Automaton
removeUselessStates automaton = automaton { FA.f_states = validStates
                                          , FA.f_acceptingStates = reducedAccepting
                                          , FA.f_transitions = reducedTransitions }
  where
    validStates :: [FA.StateID]
    validStates = ShortestString.findReachable automaton
    reducedTransitions :: [FA.Transition]
    reducedTransitions = filter (\t -> (FA.f_fromState t) `List.elem` validStates) (FA.f_transitions automaton)
    reducedAccepting :: [FA.StateID]
    reducedAccepting = filter (\a -> a `List.elem` validStates) (FA.f_acceptingStates automaton)


-- -------------------------------------------------------------------
-- Modify DFA
-- -------------------------------------------------------------------

complement :: DFA -> DFA
complement dfa = dfa { FA.f_acceptingStates = invertedAcceptStates }
  where
    invertedAcceptStates = filter (\s -> not (s `List.elem` originalAcceptStates)) (FA.f_states dfa)
    originalAcceptStates = FA.f_acceptingStates dfa


