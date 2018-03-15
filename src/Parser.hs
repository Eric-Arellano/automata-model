module Parser (parseProgram) where

import Data.List
import Data.Char
import Data.Maybe
import qualified Data.Text as Text

import AutomatonTypes


type Line = String


parseProgram :: [Line] -> Maybe ProgramData
parseProgram lines = do
    let trimmed = stripWhiteSpace lines
    afterExpectAlphabet <- expect trimmed "% Input alphabet"
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
    | firstLine == value  = Just remainingLines
    | otherwise           = Nothing
  where
    (firstLine, remainingLines) = extractFirstLine lines


parseInputLanguage :: [Line] -> Maybe (InputLanguage, [Line])
parseInputLanguage lines
    | length lines == 0               = Nothing
    | all (lineLength 1) parsedLines  = Just (language, remainingLines)
    | otherwise                       = Nothing
  where
    (parsedLines, remainingLines) = extractSection lines
    language = concat parsedLines


parseTransitionFunctions :: [Line] -> Maybe ([TransitionFunction], [Line])
parseTransitionFunctions lines
    | length lines == 0               = Nothing
    | all (lineLength 5) parsedLines  = Just (functions, remainingLines)
    | otherwise                       = Nothing
  where
    (parsedLines, remainingLines) = extractSection lines
    functions = map parse parsedLines
    parse line = MakeTransitionFunction { from=(parseFrom line)
                                        , transition=(parseTransition line)
                                        , to=(parseTo line)}
    parseFrom line = digitToInt (line !! 0)
    parseTransition line = line !! 2
    parseTo line = digitToInt (line !! 4)


parseStartingState :: [Line] -> Maybe (State, [Line])
parseStartingState lines
    | length lines == 0       = Nothing
    | length firstLine == 1   = Just (state, remainingLines)
    | otherwise               = Nothing
  where
    (firstLine, remainingLines) = extractFirstLine lines
    state = digitToInt (head firstLine)


parseAcceptingStates :: [Line] -> Maybe ([State], [Line])
parseAcceptingStates lines
    | length lines == 0               = Nothing
    | all (lineLength 1) parsedLines  = Just (states, remainingLines)
    | otherwise                       = Nothing
  where
    (parsedLines, remainingLines) = extractSection lines
    states = map parse parsedLines
    parse line = digitToInt (head line)


stripWhiteSpace :: [Line] -> [Line]
stripWhiteSpace =  map (Text.unpack . Text.strip . Text.pack)

lineLength :: Int -> Line -> Bool
lineLength numChars line = length line == numChars

extractFirstLine :: [Line] -> (Line, [Line])
extractFirstLine lines = (head lines, drop 1 lines)

extractSection :: [Line] -> ([Line], [Line])
extractSection = span isCurrentSection

isCurrentSection :: Line -> Bool
isCurrentSection line = not ("%" `isPrefixOf` line || null line)
