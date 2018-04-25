module Intersection (intersection) where

import qualified Data.List      as List
import qualified Data.Maybe     as Maybe

import qualified FiniteAutomata as FA
import qualified DFA            as DFA

intersection :: DFA.DFA -> DFA.DFA -> DFA.DFA
intersection dfa1 dfa2 = DFA.renumberStates
                       . DFA.removeUselessStates
                       . convertBack
                       . initAutomaton dfa1
                       $ dfa2


type IntersectID = (FA.StateID, FA.StateID)

data IntersectAutomaton = IntersectAutomaton { f_intersectAlphabet :: FA.Alphabet
                                             , f_intersectStates :: [IntersectID]
                                             , f_intersectInitialState :: IntersectID
                                             , f_intersectAcceptingStates :: [IntersectID]
                                             , f_intersectTransitions :: [IntersectTransition]
                                             } deriving (Eq, Show)

data IntersectTransition = IntersectTransition { f_intersectFromState :: IntersectID
                                               , f_intersectInputLetter :: Char
                                               , f_intersectToState :: IntersectID
                                               } deriving (Eq, Show)

cartesianProduct :: [a] -> [b] -> [(a, b)]
cartesianProduct xs ys = [(x, y) | x <- xs, y <- ys]

initAutomaton :: DFA.DFA -> DFA.DFA -> IntersectAutomaton
initAutomaton dfa1 dfa2 = IntersectAutomaton { f_intersectAlphabet = FA.f_alphabet dfa1
                                             , f_intersectStates = newStates
                                             , f_intersectInitialState = ((FA.f_initialState dfa1), (FA.f_initialState dfa2))
                                             , f_intersectAcceptingStates = cartesianProduct (FA.f_acceptingStates dfa1) (FA.f_acceptingStates dfa2)
                                             , f_intersectTransitions = convertTransitions }
  where
    newStates = cartesianProduct (FA.f_states dfa1) (FA.f_states dfa2)
    convertTransitions = foldl (++) [] . map findTransitions $ newStates
    findTransitions :: IntersectID -> [IntersectTransition]
    findTransitions convID = map (createTransition convID) (FA.f_alphabet dfa1)
    createTransition :: IntersectID -> Char -> IntersectTransition
    createTransition intersectID char = IntersectTransition { f_intersectFromState = intersectID
                                                            , f_intersectInputLetter = char
                                                            , f_intersectToState = (getToState char dfa1 (fst intersectID),
                                                                                    getToState char dfa2 (snd intersectID)) }
    getToState :: Char -> DFA.DFA -> FA.StateID -> FA.StateID
    getToState char dfa stateID = Maybe.fromMaybe (-3)
                                . fmap FA.f_toState
                                . List.find (\t ->  (FA.f_inputLetter t) == char && (FA.f_fromState t) == stateID)
                                $ FA.f_transitions dfa


convertBack :: IntersectAutomaton -> DFA.DFA
convertBack intersectAutomaton = FA.Automaton { FA.f_alphabet = f_intersectAlphabet intersectAutomaton
                                              , FA.f_states = map convertID (f_intersectStates intersectAutomaton)
                                              , FA.f_initialState = convertID (f_intersectInitialState intersectAutomaton)
                                              , FA.f_acceptingStates = map convertID (f_intersectAcceptingStates intersectAutomaton)
                                              , FA.f_transitions = map convertTransition (f_intersectTransitions intersectAutomaton) }
  where
    convertID :: IntersectID -> FA.StateID
    convertID convID = Maybe.fromMaybe (-2) (lookup convID incrementedStateIDs)
    convertTransition :: IntersectTransition -> FA.Transition
    convertTransition intersectTransition = FA.Transition { FA.f_fromState = convertID (f_intersectFromState intersectTransition)
                                                          , FA.f_inputLetter = f_intersectInputLetter intersectTransition
                                                          , FA.f_toState = convertID (f_intersectToState intersectTransition) }
    incrementedStateIDs :: [(IntersectID, FA.StateID)]
    incrementedStateIDs = snd $ List.mapAccumL(\index conv -> (index + 1, (conv, index))) 1 allIntersectIDs
    allIntersectIDs :: [IntersectID]
    allIntersectIDs = List.sort (f_intersectStates intersectAutomaton)