module FiniteAutomata
  ( Automaton(..)
  , StateID
  , State(..)
  , Transition(..)
  , getInitialState
  , getAcceptingStates
  , getTransitions
  , toDFA
  , isNFA
  , isDFA
  , complement
  , intersection
  ) where


import qualified Data.List      as List
import qualified Data.Maybe     as Maybe
import qualified Control.Monad  as Monad
import qualified Data.Monoid    as Monoid
import qualified Data.Ord       as Ord

data Automaton = Automaton { alphabet :: Alphabet
                           , states :: [State]
                           } deriving (Show, Eq)

type Alphabet = [Char]

type StateID = Int
data State = State { stateID :: StateID
                   , isInitial :: Bool
                   , isAccepting :: Bool
                   , transitions :: [Transition]
                   } deriving (Show, Eq)

data Transition = Transition { fromState :: State
                             , inputLetter :: Char
                             , toState :: State
                             } deriving (Show, Eq)

type DFA = Automaton


-- -------------------------------------------------------------------
-- Getters
-- -------------------------------------------------------------------

getInitialState :: [State] -> Maybe State
getInitialState = List.find (\state -> isInitial state == True)

getAcceptingStates :: [State] -> [State]
getAcceptingStates = filter (\state -> isAccepting state == True)

getTransitions :: [State] -> [Transition]
getTransitions = foldl (++) [] . map transitions


-- -------------------------------------------------------------------
-- Check automaton  type
-- -------------------------------------------------------------------

isDFA :: Automaton -> Bool
isDFA automaton = all everyInputDefined (states automaton)
  where
    everyInputDefined :: State -> Bool
    everyInputDefined state = all (hasLetter state) (alphabet automaton)
    hasLetter :: State -> Char -> Bool
    hasLetter state letter = letter `List.elem` (transitionLetters state)
    transitionLetters :: State -> [Char]
    transitionLetters state = map inputLetter (transitions state)


isNFA :: Automaton -> Bool
isNFA = not . isDFA


-- -------------------------------------------------------------------
-- Convert NFA to DFA
-- -------------------------------------------------------------------

type ConvID = [StateID]
data ConvState = ConvState { convID :: ConvID
                           , convIsInitial :: Bool
                           , convIsAccepting :: Bool
                           , convTransitions :: [ConvTransition]
                           } deriving (Eq, Show)

data ConvTransition = ConvTransition { convFromState :: ConvState
                                     , convInputLetter :: Char
                                     , convToState :: ConvState
                                     } deriving (Eq, Show)

toDFA :: Automaton -> DFA
toDFA automaton
  | isDFA automaton   = automaton
  | otherwise         = Automaton { alphabet = alphabet automaton
                                  , states = convertBackStates
                                           . removeUselessStates
                                           . addTransitionFunctions originalAlphabet originalStates
                                           . addAcceptingStates originalStates
                                           . addInitialState originalStates
                                           . initConvStates
                                           $ originalStates
                                  }
  where
    originalAlphabet = alphabet automaton
    originalStates = states automaton


initConvStates :: [State] -> [ConvState]
initConvStates = map initState . powerset . map stateID
  where
    initState :: [StateID] -> ConvState
    initState stateIDs = ConvState { convID = stateIDs
                                   , convIsInitial = False
                                   , convIsAccepting = False
                                   , convTransitions = [] }
    powerset :: [a] -> [[a]]
    powerset = Monad.filterM (const [True, False])


addInitialState :: [State] -> [ConvState] -> [ConvState]
addInitialState states = map (\convState -> if ((convID convState) == initialStateID)
                                            then convState { convIsInitial = True }
                                            else convState)
  where
    initialStateID :: ConvID
    initialStateID = case getInitialState states of
                        Just i  -> [stateID i]
                        Nothing -> [-1]


addAcceptingStates :: [State] -> [ConvState] -> [ConvState]
addAcceptingStates states = map (\convState -> if not . null . acceptingIntersection $ convState
                                               then convState { convIsAccepting = True }
                                               else convState)
  where
    acceptingIntersection :: ConvState -> ConvID
    acceptingIntersection convState = (convID convState) `List.intersect` acceptingStateIDs
    acceptingStateIDs :: [StateID]
    acceptingStateIDs = map stateID (getAcceptingStates states)


addTransitionFunctions :: Alphabet -> [State] -> [ConvState] -> [ConvState]
addTransitionFunctions alphabet states convStates = map addConvTransitions convStates
  where
    addConvTransitions :: ConvState -> ConvState
    addConvTransitions convState = convState { convTransitions = findConvTransitions convState }
    findConvTransitions :: ConvState -> [ConvTransition]
    findConvTransitions convState = concat $ map (findConvTransitionsWithLetter convState) alphabet
    findConvTransitionsWithLetter :: ConvState -> Char -> [ConvTransition]
    findConvTransitionsWithLetter convState char
       | null (getTransitionsForLetterAndState char convState)  = [createTransition convState char (Maybe.fromJust (getNullState convStates))]
       | otherwise                                              = [createTransition convState char (Maybe.fromJust (getConvState (reduceFromStateToID (getTransitionsForLetterAndState char convState)) convStates))]
    createTransition :: ConvState -> Char -> ConvState -> ConvTransition
    createTransition from letter to = ConvTransition { convFromState = from
                                                     , convInputLetter = letter
                                                     , convToState = to }
    reduceFromStateToID :: [Transition] -> ConvID
    reduceFromStateToID = List.sort . map stateID . map toState
    getTransitionsForLetterAndState :: Char -> ConvState -> [Transition]
    getTransitionsForLetterAndState char convState = filter (\transition -> stateID (fromState transition) `elem` (convID convState))
                                                            (getTransitionsForLetter char)
    getTransitionsForLetter :: Char -> [Transition]
    getTransitionsForLetter char = filter (\transition ->  (inputLetter transition) == char) (getTransitions states)


removeUselessStates :: [ConvState] -> [ConvState]
removeUselessStates convStates = filter (\cs -> convIsInitial cs || isReachable cs) convStates
  where
    isReachable :: ConvState -> Bool
    isReachable convState = convID convState `elem` allToStates
    allToStates :: [ConvID]
    allToStates = map convID . map convToState $ allTransitions
    allTransitions :: [ConvTransition]
    allTransitions = foldl (++) [] . map convTransitions $ convStates


convertBackStates :: [ConvState] -> [State]
convertBackStates allConvStates = map convertState allConvStates
  where
    convertState :: ConvState -> State
    convertState convState = State { stateID =  Maybe.fromMaybe (-1) (lookup (convID convState) incrementedStateIDs)
                                   , isInitial = convIsInitial convState
                                   , isAccepting = convIsAccepting convState
                                   , transitions = map convertTransition (convTransitions convState) }
    convertTransition :: ConvTransition -> Transition
    convertTransition convTransition = Transition { fromState = convertState (convFromState convTransition)
                                                  , inputLetter = convInputLetter convTransition
                                                  , toState = convertState (convToState convTransition) }
    incrementedStateIDs :: [(ConvID, StateID)]
    incrementedStateIDs = snd $ List.mapAccumL(\index conv -> (index + 1, (conv, index))) 0 allConvIDs
    allConvIDs :: [ConvID]
    allConvIDs = List.sortBy (Monoid.mconcat [Ord.comparing length, compare])
               . map convID $ allConvStates

getNullState :: [ConvState] -> Maybe ConvState
getNullState = List.find (\state -> convID state == [])

getConvState :: ConvID -> [ConvState] -> Maybe ConvState
getConvState targetID = List.find (\state -> convID state == targetID)


-- -------------------------------------------------------------------
-- Modify DFA
-- -------------------------------------------------------------------

complement :: DFA -> DFA
complement dfa = dfa { states = map invertAccept (states dfa) }
  where
    invertAccept :: State -> State
    invertAccept state = state { isAccepting = not (isAccepting state)}


intersection :: DFA -> DFA -> DFA
intersection = undefined
