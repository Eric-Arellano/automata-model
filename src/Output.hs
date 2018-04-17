module Output (automaton) where

import qualified Data.List      as List
import qualified FiniteAutomata as FA

automaton :: FA.Automaton -> [String]
automaton automaton =  alphabet automaton
                    ++ ["% Specification automaton"]
                    ++ transitions automaton
                    ++ initialState automaton
                    ++ finalStates automaton

alphabet :: FA.Automaton -> [String]
alphabet automaton = ["% Input alphabet"] ++ getAlphabet
  where
   getAlphabet :: [String]
   getAlphabet = map (\l -> [l]) (FA.alphabet automaton)

transitions :: FA.Automaton -> [String]
transitions automaton = ["% Transition function"] ++ map outputTransition allTransitions
  where
    outputTransition :: FA.Transition -> String
    outputTransition transition = show (FA.stateID (FA.fromState transition))
                                ++ " "
                                ++ [FA.inputLetter transition]
                                ++ " "
                                ++ show (FA.stateID (FA.toState transition))
    allTransitions :: [FA.Transition]
    allTransitions = List.sortBy (\t1 t2 -> compare (FA.stateID (FA.fromState t1)) (FA.stateID (FA.fromState t2)))
                   . foldl (++) [] . map FA.transitions
                   $ FA.states automaton

initialState :: FA.Automaton -> [String]
initialState automaton = ["% Initial state", getInitialStateID]
  where
    getInitialStateID :: String
    getInitialStateID = show (FA.stateID getInitialState)
    getInitialState :: FA.State
    getInitialState = head . filter (FA.isInitial) $ FA.states automaton

finalStates :: FA.Automaton -> [String]
finalStates automaton = ["% Final states"] ++ getFinalStateIDs
  where
    getFinalStateIDs :: [String]
    getFinalStateIDs = map show . map FA.stateID $ getFinalStates
    getFinalStates :: [FA.State]
    getFinalStates = filter (FA.isAccepting) (FA.states automaton)