module Parser (parseProgram) where

import qualified Data.List  as List
import qualified Data.Char  as Char
import qualified Data.Maybe as Maybe
import qualified Data.Text  as Text

import qualified FiniteAutomata as FA


parseProgram :: [Line] -> Maybe FA.Automaton
parseProgram lines = do
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
parseToPrimitives lines = do
    afterExpectAlphabet <- expect lines "% Input alphabet"
    (alphabet, afterAlphabet) <- parseAlphabet afterExpectAlphabet
    afterExpectSpec <- expect afterAlphabet "% Specification automaton"
    afterExpectTransition <- expect afterExpectSpec "% Transition function"
    (transitions, afterTransition) <- parseTransitions afterExpectTransition
    afterExpectInitial <- expect afterTransition "% Initial state"
    (startingState, afterStartingState) <- parseStartingState afterExpectInitial
    afterExpectingAcceptingStates <- expect afterStartingState "% Final states"
    (acceptingStates, _) <- parseAcceptingStates afterExpectingAcceptingStates
    return InputData { alphabet=alphabet
                     , transitions=transitions
                     , startingState=startingState
                     , acceptingStates=acceptingStates}


expect :: [Line] -> String -> Maybe [Line]
expect lines value = wrap lines isValid remainingLines
  where
    (firstLine, remainingLines) = extractFirstLine lines
    isValid :: Bool
    isValid = firstLine == value


parseAlphabet :: [Line] -> Maybe (Alphabet, [Line])
parseAlphabet lines = wrap lines isValid (alphabet, remainingLines)
  where
    (parsedLines, remainingLines) = extractSection lines
    isValid :: Bool
    isValid = all (lineLength 1) parsedLines
    alphabet :: Alphabet
    alphabet = concat parsedLines


parseTransitions :: [Line] -> Maybe ([Transition], [Line])
parseTransitions lines = wrap lines isValid (transitions, remainingLines)
  where
    (parsedLines, remainingLines) = extractSection lines
    isValid :: Bool
    isValid = all (lineLength 5) parsedLines
    transitions :: [Transition]
    transitions = map parse parsedLines
    parse :: Line -> Transition
    parse line = Transition { fromState=(parseFrom line)
                            , letter=(parseLetter line)
                            , toState=(parseTo line)}
    parseFrom :: Line -> StateID
    parseFrom line = Char.digitToInt (line !! 0)
    parseLetter :: Line -> Char
    parseLetter line = line !! 2
    parseTo :: Line -> StateID
    parseTo line = Char.digitToInt (line !! 4)


parseStartingState :: [Line] -> Maybe (StateID, [Line])
parseStartingState lines = wrap lines isValid (state, remainingLines)
  where
    (firstLine, remainingLines) = extractFirstLine lines
    isValid :: Bool
    isValid = length firstLine == 1
    state :: StateID
    state = Char.digitToInt (head firstLine)


parseAcceptingStates :: [Line] -> Maybe ([StateID], [Line])
parseAcceptingStates lines = wrap lines isValid (states, remainingLines)
  where
    (parsedLines, remainingLines) = extractSection lines
    isValid :: Bool
    isValid = all (lineLength 1) parsedLines
    states :: [StateID]
    states = map parse parsedLines
    parse :: Line -> StateID
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
    newState stateID = FA.State { FA.number = stateID
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
    add state = state { FA.transitions = findAll (FA.number state)}
    findAll :: StateID -> [FA.Transition]
    findAll stateID = (convertTransitions states) . filter (matchingIDs stateID) $ transitions
    matchingIDs :: StateID -> Transition -> Bool
    matchingIDs stateID transition = (fromState transition) == stateID


convertTransitions :: [FA.State] -> [Transition] -> [FA.Transition]
convertTransitions states = map convert
  where
    convert :: Transition -> FA.Transition
    convert transition = FA.Transition { FA.inputLetter = (letter transition)
                                        , FA.toState = getState (toState transition) }
    getState :: StateID -> FA.State
    getState stateID = Maybe.fromJust (List.find (\state -> (FA.number state) == stateID) states)


addInitial :: StateID -> [FA.State] -> [FA.State]
addInitial stateID = map (\state -> if ((FA.number state) == stateID)
                                    then state { FA.isInitial = True }
                                    else state)


addAccepting :: [StateID] -> [FA.State] -> [FA.State]
addAccepting stateIDs = map (\state -> if ((FA.number state) `elem` stateIDs)
                                     then state { FA.isAccepting = True }
                                     else state)
