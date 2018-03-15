module Main where
import Data.List
import Data.Char

-- TODO: refactor to not use !!. How can this be made more idiomatic?
-- TODO: refactor to use parse function

main :: IO ()
main = do
  contents <- readFile "hello.txt"
  let lns = lines contents

  let hasInput = expect lns "% Input alphabet" 1
  let (inputLanguage, lineAfterAlphabet) = parseInputLanguage lns "" 2

  let hasSpec = expect lns "% Specification automaton" lineAfterAlphabet

  let hasTransition = expect lns "% Transition function" (lineAfterAlphabet + 1)
  let (transitionFunctions, lineAfterTransition) = parseTransitionFunctions lns [] (lineAfterAlphabet + 2)

  let hasStarting = expect lns "% Initial state" lineAfterTransition
  let (startingState, lineAfterStartingState) = parseStartingState lns (lineAfterTransition + 1)

  let hasAccepting = expect lns "% Final states" lineAfterStartingState
  let (acceptingStates, _) = parseAcceptingStates lns [] (lineAfterStartingState + 1)

  let programData = ProgramData { inputLanguage=inputLanguage,
                                  transitionFunctions=transitionFunctions,
                                  startingState=startingState,
                                  acceptingStates=acceptingStates}

  print programData


type Line = String
type LineNumber = Int

type InputLanguage = String
type State = Int
data TransitionFunction = TransitionFunction { from :: State,
                                               transition :: Char,
                                               to :: State } deriving (Show)

data ProgramData = ProgramData { inputLanguage :: InputLanguage,
                                 transitionFunctions :: [TransitionFunction],
                                 startingState :: State,
                                 acceptingStates :: [State] } deriving (Show)


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
          convertLine = TransitionFunction {from=fromState,
                                            transition=transitionChar,
                                            to=toState}
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