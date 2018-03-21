module Output (automaton) where


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
    outputTransition transition = show (FA.number (FA.fromState transition))
                                ++ " "
                                ++ [FA.inputLetter transition]
                                ++ " "
                                ++ show (FA.number (FA.toState transition))
    allTransitions :: [FA.Transition]
    allTransitions = foldl (++) [] . map FA.transitions $ FA.states automaton

initialState :: FA.Automaton -> [String]
initialState automaton = ["% Initial state", getInitialStateID]
  where
    getInitialStateID :: String
    getInitialStateID = show (FA.number getInitialState)
    getInitialState :: FA.State
    getInitialState = head . filter (FA.isInitial) $ FA.states automaton

finalStates :: FA.Automaton -> [String]
finalStates automaton = ["% Final states"] ++ getFinalStateIDs
  where
    getFinalStateIDs :: [String]
    getFinalStateIDs = map show . map FA.number $ getFinalStates
    getFinalStates :: [FA.State]
    getFinalStates = filter (FA.isAccepting) (FA.states automaton)