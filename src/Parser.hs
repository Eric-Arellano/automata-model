module Parser (parseProgram) where

import Data.List
import Data.Char
import AutomatonTypes


type Line = String

-- TODO: trim trailing whitespace
-- TODO: use Maybe

parseProgram :: [Line] -> ProgramData
parseProgram lines =
   let (hasInput, afterExpectAlphabet) = expect lines "% Input alphabet"
       (inputLanguage, afterAlphabet) = parseInputLanguage afterExpectAlphabet
       (hasSpec, afterExpectSpec) = expect afterAlphabet "% Specification automaton"
       (hasTransition, afterExpectTransition) = expect afterExpectSpec "% Transition function"
       (transitionFunctions, afterTransition) = parseTransitionFunctions afterExpectTransition
       (hasStarting, afterExpectInitial) = expect afterTransition "% Initial state"
       (startingState, afterStartingState) = parseStartingState afterExpectInitial
       (hasAccepting, afterExpectingAcceptingStates) = expect afterStartingState "% Final states"
       (acceptingStates, _) = parseAcceptingStates afterExpectingAcceptingStates
   in MakeProgramData { inputLanguage=inputLanguage
                      , transitionFunctions=transitionFunctions
                      , startingState=startingState
                      , acceptingStates=acceptingStates}


expect :: [Line] -> String -> (Bool, [Line])
expect lines value = (first == value, remainingLines)
    where first = head lines  -- TODO: head is dangerous. Crashes if empty
          remainingLines = drop 1 lines


parseInputLanguage :: [Line] -> (InputLanguage, [Line])
parseInputLanguage lines = (inputLanguage, remainingLines)
    where inputLanguage = concat (fst split)
          remainingLines = snd split
          split = span isCurrentSection lines


parseTransitionFunctions :: [Line] -> ([TransitionFunction], [Line])
parseTransitionFunctions lines = (transitionFunctions, remainingLines)
    where transitionFunctions = map convertLine (fst split)
          convertLine line = MakeTransitionFunction { from=(from line)
                                           , transition=(transition line)
                                           , to=(to line)}
          from line = digitToInt (line !! 0)
          transition line = line !! 2
          to line = digitToInt (line !! 4)
          remainingLines = snd split
          split = span isCurrentSection lines


parseStartingState :: [Line] -> (State, [Line])
parseStartingState lines = (state, remainingLines)
    where state = digitToInt (head (head lines))  -- TODO: head is dangerous. Crashes if empty
          remainingLines = drop 1 lines


parseAcceptingStates :: [Line] -> ([State], [Line])
parseAcceptingStates lines = (states, remainingLines)
    where states = map toState (fst split)
          toState line = digitToInt (head line)
          remainingLines = snd split
          split = span isCurrentSection lines


isCurrentSection :: Line -> Bool
isCurrentSection line = not ("%" `isPrefixOf` line || null line)
