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

type Alphabet = [Char]

parseAlphabet :: [Line] -> (Maybe Alphabet, [Line])
parseAlphabet input = (alphabet, remainingLines)
  where
    (_, afterHeader) = expect input "% Input alphabet"
    (parsedLines, remainingLines) = extractSection afterHeader
    alphabet :: Maybe Alphabet
    alphabet = sequence letters
    letters :: [Maybe Char]
    letters = map parse $ parsedLines
    parse :: Line -> Maybe Char
    parse line = case line of
                   x:_ -> Just x
                   []  -> Nothing


parseSpecification :: [Line] -> Alphabet -> (Maybe FA.Automaton, [Line])
parseSpecification input alphabet = (automaton, remainingLines)
    where
      (_, afterHeader) = expect input "% Specification automaton"
      (primitives, remainingLines) = parseToPrimitives afterHeader
      automaton = case primitives of
                    Just p -> Just (convertToAutomaton alphabet p)
                    Nothing -> Nothing


parseSystem :: [Line] -> Alphabet -> (Maybe FA.Automaton, [Line])
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

type StateID = Int

data Transition = Transition { f_fromState :: StateID
                             , f_letter :: Char
                             , f_toState :: StateID
                             } deriving (Show)

data InputData = InputData { f_transitions :: [Transition]
                           , f_startingState :: StateID
                           , f_acceptingStates :: [StateID]
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


parseTransitions :: [Line] -> (Maybe [Transition], [Line])
parseTransitions input = (transitions, remainingLines)
  where
    (parsedLines, remainingLines) = extractSection input
    transitions :: Maybe [Transition]
    transitions = sequence . map parse $ parsedLines
    parse :: Line -> Maybe Transition
    parse line = case (parseFrom line, parseLetter line, parseTo line) of
                      (Nothing, _, _) -> Nothing
                      (_, Nothing, _) -> Nothing
                      (_, _, Nothing) -> Nothing
                      (Just from, Just letter, Just to) -> Just Transition { f_fromState=from
                                                                             , f_letter=letter
                                                                             , f_toState=to}
    parseFrom :: Line -> Maybe StateID
    parseFrom line = case line of
                       x:_ -> Just (Char.digitToInt x)  -- TODO: digitToInt not type safe
                       []  -> Nothing
    parseLetter :: Line -> Maybe Char
    parseLetter line = case drop 2 line of
                         x:_ -> Just x
                         []  -> Nothing
    parseTo :: Line -> Maybe StateID
    parseTo line = case drop 4 line of
                     x:_ -> Just (Char.digitToInt x)
                     []  -> Nothing


parseStartingState :: [Line] -> (Maybe StateID, [Line])
parseStartingState input = (state, remainingLines)
  where
    (firstLine, remainingLines) = extractFirstLine input
    state :: Maybe StateID
    state = case firstLine of
               Just (x:_) -> Just (Char.digitToInt x)
               _          -> Nothing


parseAcceptingStates :: [Line] -> (Maybe [StateID], [Line])
parseAcceptingStates input = (states, remainingLines)
  where
    (parsedLines, remainingLines) = extractSection input
    states :: Maybe [StateID]
    states = sequence . map parse $ parsedLines
    parse :: Line -> Maybe StateID
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
----------------------------------------------------------------------

convertToAutomaton :: Alphabet -> InputData -> FA.Automaton
convertToAutomaton alphabet input
    = FA.Automaton { FA.f_alphabet = alphabet
                   , FA.f_states = states }
  where
    states = addAccepting (f_acceptingStates input)
           . addInitial (f_startingState input)
           . addTransitions (f_transitions input)
           . initStates $ input

initStates :: InputData -> [FA.State]
initStates input = map newState uniqueIDs
  where
    newState :: StateID -> FA.State
    newState stateID = FA.State { FA.f_stateID = stateID
                                , FA.f_isInitial = False
                                , FA.f_isAccepting = False
                                , FA.f_transitions = []}
    uniqueIDs :: [StateID]
    uniqueIDs = List.nub (getIDs f_fromState ++ getIDs f_toState)
    getIDs :: (Transition -> StateID) -> [StateID]
    getIDs target = map target . f_transitions $ input


addTransitions :: [Transition] -> [FA.State] -> [FA.State]
addTransitions transitions states = map add states
  where
    add :: FA.State -> FA.State
    add state = state { FA.f_transitions = findAll (FA.f_stateID state)}
    findAll :: StateID -> [FA.Transition]
    findAll stateID = convertTransitions . filter (matchingIDs stateID) $ transitions
    matchingIDs :: StateID -> Transition -> Bool
    matchingIDs stateID transition = (f_fromState transition) == stateID


convertTransitions :: [Transition] -> [FA.Transition]
convertTransitions = map convert
  where
    convert :: Transition -> FA.Transition
    convert transition = FA.Transition { FA.f_fromState = f_fromState transition
                                       , FA.f_inputLetter = (f_letter transition)
                                       , FA.f_toState = f_toState transition }


addInitial :: StateID -> [FA.State] -> [FA.State]
addInitial stateID = map (\state -> if ((FA.f_stateID state) == stateID)
                                    then state { FA.f_isInitial = True }
                                    else state)


addAccepting :: [StateID] -> [FA.State] -> [FA.State]
addAccepting stateIDs = map (\state -> if ((FA.f_stateID state) `elem` stateIDs)
                                       then state { FA.f_isAccepting = True }
                                       else state)
