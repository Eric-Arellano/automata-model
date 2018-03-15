module Parser (parseProgram) where

import Data.List
import Data.Char
import AutomatonTypes


type Line = String
type LineNumber = Int

-- TODO: refactor to not use !!. How can this be made more idiomatic?
-- TODO: maybe convert the Line data type to be (String, LineNumber)
-- TODO: Zac's idea of first mapping every line to be Line, and then using map and filter

-- TODO: return Maybe ProgramData type, and check that everything is valid
parseProgram :: [Line] -> ProgramData
parseProgram lns =
   let hasInput = expect lns "% Input alphabet" 1
       (inputLanguage, lineAfterAlphabet) = parseInputLanguage lns "" 2
       hasSpec = expect lns "% Specification automaton" lineAfterAlphabet
       hasTransition = expect lns "% Transition function" (lineAfterAlphabet + 1)
       (transitionFunctions, lineAfterTransition) = parseTransitionFunctions lns [] (lineAfterAlphabet + 2)
       hasStarting = expect lns "% Initial state" lineAfterTransition
       (startingState, lineAfterStartingState) = parseStartingState lns (lineAfterTransition + 1)
       hasAccepting = expect lns "% Final states" lineAfterStartingState
       (acceptingStates, _) = parseAcceptingStates lns [] (lineAfterStartingState + 1)
   in MakeProgramData { inputLanguage=inputLanguage
                  , transitionFunctions=transitionFunctions
                  , startingState=startingState
                  , acceptingStates=acceptingStates}


expect :: [Line] -> String -> LineNumber -> Bool
expect lines value lineNumber = line == value
    where line = lines !! (lineNumber - 1)


parseInputLanguage :: [Line] -> InputLanguage -> LineNumber -> (InputLanguage, LineNumber)
parseInputLanguage lines language lineNumber
    | length line == 1 = parseInputLanguage lines (language ++ line) (lineNumber + 1)
    | "%" `isPrefixOf` line = (language, lineNumber)
    | otherwise = (language, lineNumber)
    where line = lines !! (lineNumber - 1)


parseTransitionFunctions :: [Line] -> [TransitionFunction] -> LineNumber -> ([TransitionFunction], LineNumber)
parseTransitionFunctions lines functions lineNumber
    | length line == 5 = parseTransitionFunctions lines (functions ++ [convertLine]) (lineNumber + 1)
    | "%" `isPrefixOf` line = (functions, lineNumber)
    | otherwise = (functions, lineNumber)
    where line = lines !! (lineNumber - 1)
          convertLine = MakeTransitionFunction { from=fromState
                                           , transition=transitionChar
                                           , to=toState}
          fromState = digitToInt (line !! 0)
          transitionChar = line !! 2
          toState = digitToInt (line !! 4)


parseStartingState :: [Line] -> LineNumber -> (State, LineNumber)
parseStartingState lines lineNumber = (state, lineNumber + 1)
    where line = lines !! (lineNumber - 1)
          state = digitToInt (line !! 0)


parseAcceptingStates :: [Line] -> [State] -> LineNumber -> ([State], LineNumber)
parseAcceptingStates lines states lineNumber
    | length line == 1 = parseAcceptingStates lines (states ++ [state]) (lineNumber + 1)
    | "%" `isPrefixOf` line = (states, lineNumber)
    | otherwise = (states, lineNumber)
    where line = lines !! (lineNumber - 1)  -- TODO: throws error when empty last line
          state = digitToInt (line !! 0)