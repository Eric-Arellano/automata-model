module Output (automata, automaton) where

import qualified Data.List      as List
import qualified Data.Monoid    as Monoid
import qualified FiniteAutomata as FA

automata :: FA.Automaton -> FA.Automaton -> [String]
automata fa1 fa2 = alphabet fa1
                 ++ automatonHelper fa1 "% Specification automaton"
                 ++ automatonHelper fa2 "% System automaton"

automaton :: FA.Automaton -> [String]
automaton fa = alphabet fa ++ automatonHelper fa "% Resulting automaton"

automatonHelper :: FA.Automaton -> String -> [String]
automatonHelper fa name = [name]
                        ++ transitions fa
                        ++ initialState fa
                        ++ finalStates fa

alphabet :: FA.Automaton -> [String]
alphabet fa = ["% Input alphabet"] ++ outputAlphabet
  where
   outputAlphabet :: [String]
   outputAlphabet = map (\l -> [l]) (FA.f_alphabet fa)

transitions :: FA.Automaton -> [String]
transitions fa = ["% Transition function"] ++ map outputTransition sortedTransitions
  where
    outputTransition :: FA.Transition -> String
    outputTransition transition = show (FA.f_fromState transition)
                                ++ " "
                                ++ [FA.f_inputLetter transition]
                                ++ " "
                                ++ show (FA.f_toState transition)
    sortedTransitions :: [FA.Transition]
    sortedTransitions = List.sortBy (Monoid.mconcat [sortByFromState, sortByLetter]) (FA.f_transitions fa)
    sortByFromState :: FA.Transition -> FA.Transition -> Ordering
    sortByFromState t1 t2 = compare (FA.f_fromState t1) (FA.f_fromState t2)
    sortByLetter :: FA.Transition -> FA.Transition -> Ordering
    sortByLetter t1 t2 = compare (FA.f_inputLetter t1) (FA.f_inputLetter t2)

initialState :: FA.Automaton -> [String]
initialState fa = ["% Initial state", show (FA.f_initialState fa)]

finalStates :: FA.Automaton -> [String]
finalStates fa = ["% Final states"] ++ map show sortedFinalStates
  where
    sortedFinalStates :: [FA.StateID]
    sortedFinalStates = List.sort (FA.f_acceptingStates fa)
