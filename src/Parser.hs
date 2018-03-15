module Parser (parseProgram) where

import Data.List
import Data.Char
import Data.Maybe

import AutomatonTypes


type Line = String

-- TODO: trim trailing whitespace

parseProgram :: [Line] -> Maybe ProgramData
parseProgram lines = do
    afterExpectAlphabet <- expect lines "% Input alphabet"
    (inputLanguage, afterAlphabet) <- parseInputLanguage afterExpectAlphabet
    afterExpectSpec <- expect afterAlphabet "% Specification automaton"
    afterExpectTransition <- expect afterExpectSpec "% Transition function"
    (transitionFunctions, afterTransition) <- parseTransitionFunctions afterExpectTransition
    afterExpectInitial <- expect afterTransition "% Initial state"
    (startingState, afterStartingState) <- parseStartingState afterExpectInitial
    afterExpectingAcceptingStates <- expect afterStartingState "% Final states"
    (acceptingStates, _) <- parseAcceptingStates afterExpectingAcceptingStates
    return MakeProgramData { inputLanguage=inputLanguage
                      , transitionFunctions=transitionFunctions
                      , startingState=startingState
                      , acceptingStates=acceptingStates}


expect :: [Line] -> String -> Maybe [Line]
expect lines value
    | length lines == 0   = Nothing
    | first == value      = Just remainingLines
    | otherwise           = Nothing
  where
    first = head lines
    remainingLines = drop 1 lines


parseInputLanguage :: [Line] -> Maybe (InputLanguage, [Line])
parseInputLanguage lines
    | all singleChar languageLines  = Just (language, remainingLines)
    | otherwise                     = Nothing
  where
    singleChar line = length line == 1
    language = concat languageLines
    languageLines = fst split
    remainingLines = snd split
    split = span isCurrentSection lines


parseTransitionFunctions :: [Line] -> Maybe ([TransitionFunction], [Line])
parseTransitionFunctions lines
    | all fiveChars functionLines   = Just (functions, remainingLines)
    | otherwise                     = Nothing
  where
    fiveChars line = length line == 5
    functions = map parse functionLines
    functionLines = fst split
    parse line = MakeTransitionFunction { from=(from line)
                                        , transition=(transition line)
                                        , to=(to line)}
    from line = digitToInt (line !! 0)
    transition line = line !! 2
    to line = digitToInt (line !! 4)
    remainingLines = snd split
    split = span isCurrentSection lines


parseStartingState :: [Line] -> Maybe (State, [Line])
parseStartingState lines
    | length lines == 0   = Nothing
    | length first == 1   = Just (state, remainingLines)
    | otherwise           = Nothing
  where
    state = digitToInt (head first)
    first = head lines
    remainingLines = drop 1 lines


parseAcceptingStates :: [Line] -> Maybe ([State], [Line])
parseAcceptingStates lines
    | all singleChar stateLines   = Just (states, remainingLines)
    | otherwise                   = Nothing
  where
    singleChar line = length line == 1
    states = map toState stateLines
    stateLines = fst split
    toState line = digitToInt (head line)
    remainingLines = snd split
    split = span isCurrentSection lines


isCurrentSection :: Line -> Bool
isCurrentSection line = not ("%" `isPrefixOf` line || null line)
