module Output (automaton) where

import qualified Data.List      as List
import qualified Data.Monoid    as Monoid
import qualified FiniteAutomata as FA

automaton :: FA.Automaton -> [String]
automaton fa =  alphabet fa
             ++ ["% Specification automaton"]
             ++ transitions fa
             ++ initialState fa
             ++ finalStates fa

alphabet :: FA.Automaton -> [String]
alphabet fa = ["% Input alphabet"] ++ getAlphabet
  where
   getAlphabet :: [String]
   getAlphabet = map (\l -> [l]) (FA.f_alphabet fa)

transitions :: FA.Automaton -> [String]
transitions fa = ["% Transition function"] ++ map outputTransition allTransitions
  where
    outputTransition :: FA.Transition -> String
    outputTransition transition = show (FA.f_stateID (FA.f_fromState transition))
                                ++ " "
                                ++ [FA.f_inputLetter transition]
                                ++ " "
                                ++ show (FA.f_stateID (FA.f_toState transition))
    allTransitions :: [FA.Transition]
    allTransitions = List.sortBy (Monoid.mconcat [sortByFromState, sortByLetter])
                   . foldl (++) [] . map FA.f_transitions
                   $ FA.f_states fa
    sortByFromState :: FA.Transition -> FA.Transition -> Ordering
    sortByFromState t1 t2 = compare (FA.f_stateID (FA.f_fromState t1)) (FA.f_stateID (FA.f_fromState t2))
    sortByLetter :: FA.Transition -> FA.Transition -> Ordering
    sortByLetter t1 t2 = compare (FA.f_inputLetter t1) (FA.f_inputLetter t2)

initialState :: FA.Automaton -> [String]
initialState fa = ["% Initial state", getInitialStateID]
  where
    getInitialStateID :: String
    getInitialStateID = case getInitialState of
                          Just x -> show (FA.f_stateID x)
                          Nothing -> ""
    getInitialState :: Maybe FA.State
    getInitialState = FA.getInitialState $ FA.f_states fa

finalStates :: FA.Automaton -> [String]
finalStates fa = ["% Final states"] ++ getFinalStateIDs
  where
    getFinalStateIDs :: [String]
    getFinalStateIDs = map show . map FA.f_stateID $ getFinalStates
    getFinalStates :: [FA.State]
    getFinalStates = filter (FA.f_isAccepting) (FA.f_states fa)