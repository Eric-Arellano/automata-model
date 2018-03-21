module Output (automaton) where


import qualified FiniteAutomata as FA

automaton :: FA.Automaton -> [String]
automaton automaton =  alphabet automaton
--                  ++ transitionFunctions automaton
                    ++ ["% Specification automaton"]
                    ++ initialState automaton
                    ++ finalStates automaton

alphabet :: FA.Automaton -> [String]
alphabet automaton = ["% Input alphabet"] ++ getAlphabet
  where
   getAlphabet :: [String]
   getAlphabet = map (\l -> [l]) (FA.alphabet automaton)

transitionFunctions :: FA.Automaton -> [String]
transitionFunctions automaton = undefined

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