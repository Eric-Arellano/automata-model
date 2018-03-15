module Parser (parseProgram) where

import qualified Data.List  as List
import qualified Data.Char  as Char
import qualified Data.Text  as Text

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
expect lines value = wrap lines isValid remainingLines
  where
    (firstLine, remainingLines) = extractFirstLine lines
    isValid = firstLine == value


parseInputLanguage :: [Line] -> Maybe (InputLanguage, [Line])
parseInputLanguage lines = wrap lines isValid (language, remainingLines)
  where
    (parsedLines, remainingLines) = extractSection lines
    isValid = all (lineLength 1) parsedLines
    language = concat parsedLines


parseTransitionFunctions :: [Line] -> Maybe ([TransitionFunction], [Line])
parseTransitionFunctions lines = wrap lines isValid (functions, remainingLines)
  where
    (parsedLines, remainingLines) = extractSection lines
    isValid = all (lineLength 5) parsedLines
    functions = map parse parsedLines
    parse line = MakeTransitionFunction { from=(parseFrom line)
                                        , transition=(parseTransition line)
                                        , to=(parseTo line)}
    parseFrom line = Char.digitToInt (line !! 0)
    parseTransition line = line !! 2
    parseTo line = Char.digitToInt (line !! 4)


parseStartingState :: [Line] -> Maybe (State, [Line])
parseStartingState lines = wrap lines isValid (state, remainingLines)
  where
    (firstLine, remainingLines) = extractFirstLine lines
    isValid = length firstLine == 1
    state = Char.digitToInt (head firstLine)


parseAcceptingStates :: [Line] -> Maybe ([State], [Line])
parseAcceptingStates lines = wrap lines isValid (states, remainingLines)
  where
    (parsedLines, remainingLines) = extractSection lines
    isValid = all (lineLength 1) parsedLines
    states = map parse parsedLines
    parse line = Char.digitToInt (head line)


stripWhiteSpace :: [Line] -> [Line]
stripWhiteSpace =  map (Text.unpack . Text.strip . Text.pack)

lineLength :: Int -> Line -> Bool
lineLength numChars line = length line == numChars

extractFirstLine :: [Line] -> (Line, [Line])
extractFirstLine lines = (head lines, drop 1 lines)

extractSection :: [Line] -> ([Line], [Line])
extractSection = span isCurrentSection

wrap :: [Line] -> Bool -> success -> Maybe success
wrap lines isValid success
  | length lines == 0   = Nothing
  | isValid             = Just success
  | otherwise           = Nothing

isCurrentSection :: Line -> Bool
isCurrentSection line = not ("%" `List.isPrefixOf` line || null line)
