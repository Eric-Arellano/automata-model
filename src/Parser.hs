module Parser (parseSpecification, parseSystem) where

import qualified Data.List  as List
import qualified Data.Char  as Char
import qualified Data.Maybe as Maybe
import qualified Data.Text  as Text

import qualified FiniteAutomata as FA

-- TODO: implement parsing the two automata
parseSpecification :: [Line] -> Maybe FA.Automaton
parseSpecification lines = do
    let trimmed = stripWhiteSpace lines
    input <- parseToPrimitives trimmed
    return $ convertToAutomaton input

parseSystem :: [Line] -> Maybe FA.Automaton
parseSystem lines = do
    let trimmed = stripWhiteSpace lines
    input <- parseToPrimitives trimmed
    return $ convertToAutomaton input

-- -------------------------------------------------------------------
-- Parse to primitive data representation
-- --------------------------------------------------------------------

type Line = String

type Alphabet = [Char]
type StateID = Int

data Transition = Transition { fromState :: StateID
                             , letter :: Char
                             , toState :: StateID
                             } deriving (Show)

data InputData = InputData { alphabet :: Alphabet
                           , transitions :: [Transition]
                           , startingState :: StateID
                           , acceptingStates :: [StateID]
                           } deriving (Show)


parseToPrimitives :: [Line] -> Maybe InputData
parseToPrimitives lines = case (alphabet, transitions, startingState, acceptingStates) of
                            (Nothing, _, _, _) -> Nothing
                            (_, Nothing, _, _) -> Nothing
                            (_, _, Nothing, _) -> Nothing
                            (Just a, Just t, Just s, Nothing) -> Just InputData { alphabet=a
                                                                                , transitions=t
                                                                                , startingState=s
                                                                                , acceptingStates=[]}
                            (Just a, Just t, Just s, Just f) -> Just InputData { alphabet=a
                                                                                , transitions=t
                                                                                , startingState=s
                                                                                , acceptingStates=f}
  where
    (_, afterExpectAlphabet) = expect lines "% Input alphabet"
    (alphabet, afterAlphabet) = parseAlphabet afterExpectAlphabet
    (_, afterExpectSpec) = expect afterAlphabet "% Specification automaton"
    (_, afterExpectTransition) = expect afterExpectSpec "% Transition function"
    (transitions, afterTransition) = parseTransitions afterExpectTransition
    (_, afterExpectInitial) = expect afterTransition "% Initial state"
    (startingState, afterStartingState) = parseStartingState afterExpectInitial
    (_, afterExpectingAcceptingStates) = expect afterStartingState "% Final states"
    (acceptingStates, _) = parseAcceptingStates afterExpectingAcceptingStates


expect :: [Line] -> Line -> (Maybe Line, [Line])
expect lines value = case firstLine of
                       Just l
                          | l == value -> (firstLine, remainingLines)
                          | otherwise  -> (Nothing, remainingLines)
                       _  -> (Nothing, remainingLines)
  where
    (firstLine, remainingLines) = extractFirstLine lines


parseAlphabet :: [Line] -> (Maybe Alphabet, [Line])
parseAlphabet lines = (alphabet, remainingLines)
  where
    (parsedLines, remainingLines) = extractSection lines
    alphabet :: Maybe Alphabet
    alphabet = sequence letters
    letters :: [Maybe Char]
    letters = map parse $ parsedLines
    parse :: Line -> Maybe Char
    parse line = case line of
                   x:_ -> Just x
                   []  -> Nothing


parseTransitions :: [Line] -> (Maybe [Transition], [Line])
parseTransitions lines = (transitions, remainingLines)
  where
    (parsedLines, remainingLines) = extractSection lines
    transitions :: Maybe [Transition]
    transitions = sequence . map parse $ parsedLines
    parse :: Line -> Maybe Transition
    parse line = case (parseFrom line, parseLetter line, parseTo line) of
                      (Nothing, _, _) -> Nothing
                      (_, Nothing, _) -> Nothing
                      (_, _, Nothing) -> Nothing
                      (Just from, Just letter, Just to) -> Just Transition { fromState=from
                                                                           , letter=letter
                                                                           , toState=to}
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
parseStartingState lines = (state, remainingLines)
  where
    (firstLine, remainingLines) = extractFirstLine lines
    state :: Maybe StateID
    state = case firstLine of
               Just (x:_) -> Just (Char.digitToInt x)
               _          -> Nothing


parseAcceptingStates :: [Line] -> (Maybe [StateID], [Line])
parseAcceptingStates lines = (states, remainingLines)
  where
    (parsedLines, remainingLines) = extractSection lines
    states :: Maybe [StateID]
    states = sequence . map parse $ parsedLines
    parse :: Line -> Maybe StateID
    parse line = case line of
                   x:_ -> Just (Char.digitToInt x)
                   []  -> Nothing


stripWhiteSpace :: [Line] -> [Line]
stripWhiteSpace =  map (Text.unpack . Text.strip . Text.pack)

extractFirstLine :: [Line] -> (Maybe Line, [Line])
extractFirstLine lines = (firstLine, drop 1 lines)
  where
    firstLine = case lines of
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

convertToAutomaton :: InputData -> FA.Automaton
convertToAutomaton input
    = FA.Automaton { FA.alphabet=(alphabet input)
                   , FA.states=states }
  where
    states = addAccepting (acceptingStates input)
           . addInitial (startingState input)
           . addTransitions (transitions input)
           . initStates $ input

initStates :: InputData -> [FA.State]
initStates input = map newState uniqueIDs
  where
    newState :: StateID -> FA.State
    newState stateID = FA.State { FA.stateID = stateID
                                , FA.isInitial = False
                                , FA.isAccepting = False
                                , FA.transitions = []}
    uniqueIDs :: [StateID]
    uniqueIDs = List.nub (getIDs fromState ++ getIDs toState)
    getIDs :: (Transition -> StateID) -> [StateID]
    getIDs target = map target . transitions $ input


addTransitions :: [Transition] -> [FA.State] -> [FA.State]
addTransitions transitions states = map add states
  where
    add :: FA.State -> FA.State
    add state = state { FA.transitions = findAll (FA.stateID state)}
    findAll :: StateID -> [FA.Transition]
    findAll stateID = (convertTransitions states) . filter (matchingIDs stateID) $ transitions
    matchingIDs :: StateID -> Transition -> Bool
    matchingIDs stateID transition = (fromState transition) == stateID


convertTransitions :: [FA.State] -> [Transition] -> [FA.Transition]
convertTransitions states = map convert
  where
    convert :: Transition -> FA.Transition
    convert transition = FA.Transition { FA.fromState = getState (fromState transition)
                                       , FA.inputLetter = (letter transition)
                                       , FA.toState = getState (toState transition) }
    getState :: StateID -> FA.State
    getState stateID = Maybe.fromJust (List.find (\state -> (FA.stateID state) == stateID) states)


addInitial :: StateID -> [FA.State] -> [FA.State]
addInitial stateID = map (\state -> if ((FA.stateID state) == stateID)
                                    then state { FA.isInitial = True }
                                    else state)


addAccepting :: [StateID] -> [FA.State] -> [FA.State]
addAccepting stateIDs = map (\state -> if ((FA.stateID state) `elem` stateIDs)
                                       then state { FA.isAccepting = True }
                                       else state)
