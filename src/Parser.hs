module Parser (parseAutomata) where

import qualified Data.List  as List
import qualified Data.Char  as Char
import qualified Data.Maybe as Maybe
import qualified Data.Text  as Text

import qualified FiniteAutomata as FA

type Line = String

parseAutomata :: [Line] -> (Maybe FA.Automaton, Maybe FA.Automaton)
parseAutomata input = case (alphabet, spec, system) of
                        (Nothing, _, _) -> (Nothing, Nothing)
                        (_, x, y)       -> (x, y)
  where
    trimmed = stripWhiteSpace input
    (alphabet, afterAlphabet) = parseAlphabet trimmed
    (spec, afterSpec) = parseSpecification afterAlphabet (Maybe.fromMaybe [] alphabet)
    (system, _) = parseSystem afterSpec (Maybe.fromMaybe [] alphabet)


-- -------------------------------------------------------------------
-- Parse alphabet & automata
-- --------------------------------------------------------------------

parseAlphabet :: [Line] -> (Maybe FA.Alphabet, [Line])
parseAlphabet input = (alphabet, remainingLines)
  where
    (_, afterHeader) = expect input "% Input alphabet"
    (parsedLines, remainingLines) = extractSection afterHeader
    alphabet :: Maybe FA.Alphabet
    alphabet = sequence letters
    letters :: [Maybe Char]
    letters = map parse $ parsedLines
    parse :: Line -> Maybe Char
    parse line = case line of
                   x:_ -> Just x
                   []  -> Nothing


parseSpecification :: [Line] -> FA.Alphabet -> (Maybe FA.Automaton, [Line])
parseSpecification input alphabet = (automaton, remainingLines)
    where
      (_, afterHeader) = expect input "% Specification automaton"
      (primitives, remainingLines) = parseToPrimitives afterHeader
      automaton = case primitives of
                    Just p -> Just (convertToAutomaton alphabet p)
                    Nothing -> Nothing


parseSystem :: [Line] -> FA.Alphabet -> (Maybe FA.Automaton, [Line])
parseSystem input alphabet = (automaton, remainingLines)
  where
    (_, afterHeader) = expect input "% System automaton"
    (primitives, remainingLines) = parseToPrimitives afterHeader
    automaton = case primitives of
                  Just p -> Just (convertToAutomaton alphabet p)
                  Nothing -> Nothing


-- -------------------------------------------------------------------
-- Parse to primitive data representation
-- --------------------------------------------------------------------

data InputData = InputData { f_transitions :: [FA.Transition]
                           , f_startingState :: FA.StateID
                           , f_acceptingStates :: [FA.StateID]
                           } deriving (Show)


parseToPrimitives :: [Line] -> (Maybe InputData, [Line])
parseToPrimitives input = case (transitions, startingState, acceptingStates) of
                            (Nothing, _, _) -> (Nothing, remainingLines)
                            (_, Nothing, _) -> (Nothing, remainingLines)
                            (Just t, Just s, Nothing) -> (Just InputData { f_transitions=t
                                                                         , f_startingState=s
                                                                         , f_acceptingStates=[]}
                                                          ,  remainingLines)
                            (Just t, Just s, Just f) -> (Just InputData { f_transitions=t
                                                                        , f_startingState=s
                                                                        , f_acceptingStates=f}
                                                        , remainingLines)
  where
    (_, afterExpectTransition) = expect input "% Transition function"
    (transitions, afterTransition) = parseTransitions afterExpectTransition
    (_, afterExpectInitial) = expect afterTransition "% Initial state"
    (startingState, afterStartingState) = parseStartingState afterExpectInitial
    (_, afterExpectingAcceptingStates) = expect afterStartingState "% Final states"
    (acceptingStates, remainingLines) = parseAcceptingStates afterExpectingAcceptingStates


expect :: [Line] -> Line -> (Maybe Line, [Line])
expect input value = case firstLine of
                       Just l
                          | l == value -> (firstLine, remainingLines)
                          | otherwise  -> (Nothing, remainingLines)
                       _  -> (Nothing, remainingLines)
  where
    (firstLine, remainingLines) = extractFirstLine input


parseTransitions :: [Line] -> (Maybe [FA.Transition], [Line])
parseTransitions input = (transitions, remainingLines)
  where
    (parsedLines, remainingLines) = extractSection input
    transitions :: Maybe [FA.Transition]
    transitions = sequence . map parse $ parsedLines
    parse :: Line -> Maybe FA.Transition
    parse line = case (parseFrom line, parseLetter line, parseTo line) of
                      (Nothing, _, _) -> Nothing
                      (_, Nothing, _) -> Nothing
                      (_, _, Nothing) -> Nothing
                      (Just from, Just letter, Just to) -> Just FA.Transition { FA.f_fromState=from
                                                                              , FA.f_inputLetter=letter
                                                                              , FA.f_toState=to}
    parseFrom :: Line -> Maybe FA.StateID
    parseFrom line = case line of
                       x:_ -> Just (Char.digitToInt x)  -- TODO: digitToInt not type safe
                       []  -> Nothing
    parseLetter :: Line -> Maybe Char
    parseLetter line = case drop 2 line of
                         x:_ -> Just x
                         []  -> Nothing
    parseTo :: Line -> Maybe FA.StateID
    parseTo line = case drop 4 line of
                     x:_ -> Just (Char.digitToInt x)
                     []  -> Nothing


parseStartingState :: [Line] -> (Maybe FA.StateID, [Line])
parseStartingState input = (state, remainingLines)
  where
    (firstLine, remainingLines) = extractFirstLine input
    state :: Maybe FA.StateID
    state = case firstLine of
               Just (x:_) -> Just (Char.digitToInt x)
               _          -> Nothing


parseAcceptingStates :: [Line] -> (Maybe [FA.StateID], [Line])
parseAcceptingStates input = (states, remainingLines)
  where
    (parsedLines, remainingLines) = extractSection input
    states :: Maybe [FA.StateID]
    states = sequence . map parse $ parsedLines
    parse :: Line -> Maybe FA.StateID
    parse line = case line of
                   x:_ -> Just (Char.digitToInt x)
                   []  -> Nothing


stripWhiteSpace :: [Line] -> [Line]
stripWhiteSpace =  map (Text.unpack . Text.strip . Text.pack)

extractFirstLine :: [Line] -> (Maybe Line, [Line])
extractFirstLine input = (firstLine, drop 1 input)
  where
    firstLine = case input of
                  x:_ -> Just x
                  []  -> Nothing

extractSection :: [Line] -> ([Line], [Line])
extractSection = span isCurrentSection
  where
    isCurrentSection :: Line -> Bool
    isCurrentSection line = not ("%" `List.isPrefixOf` line || null line)


-- -------------------------------------------------------------------
-- Convert to Automaton
-- -------------------------------------------------------------------

convertToAutomaton :: FA.Alphabet -> InputData -> FA.Automaton
convertToAutomaton alphabet input
    = FA.Automaton { FA.f_alphabet = alphabet
                   , FA.f_states = findStates input
                   , FA.f_initialState = f_startingState input
                   , FA.f_acceptingStates = f_acceptingStates input
                   , FA.f_transitions = f_transitions input }


findStates :: InputData -> [FA.StateID]
findStates input = uniqueIDs
  where
    uniqueIDs :: [FA.StateID]
    uniqueIDs = List.nub (getIDs FA.f_fromState
                         ++ getIDs FA.f_toState
                         ++ [f_startingState input]
                         ++ f_acceptingStates input)
    getIDs :: (FA.Transition -> FA.StateID) -> [FA.StateID]
    getIDs target = map target . f_transitions $ input
