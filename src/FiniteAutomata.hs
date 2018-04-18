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

data Automaton = Automaton { f_alphabet :: Alphabet
                           , f_states :: [State]
                           } deriving (Show, Eq)

type Alphabet = [Char]

type StateID = Int
data State = State { f_stateID :: StateID
                   , f_isInitial :: Bool
                   , f_isAccepting :: Bool
                   , f_transitions :: [Transition]
                   } deriving (Show, Eq)

data Transition = Transition { f_fromState :: StateID
                             , f_inputLetter :: Char
                             , f_toState :: StateID
                             } deriving (Show, Eq)

type DFA = Automaton


-- -------------------------------------------------------------------
-- Getters
-- -------------------------------------------------------------------

getInitialState :: [State] -> Maybe State
getInitialState = List.find (\state -> f_isInitial state == True)

getAcceptingStates :: [State] -> [State]
getAcceptingStates = filter (\state -> f_isAccepting state == True)

getTransitions :: [State] -> [Transition]
getTransitions = foldl (++) [] . map f_transitions


-- -------------------------------------------------------------------
-- Check automaton  type
-- -------------------------------------------------------------------

isDFA :: Automaton -> Bool
isDFA automaton = all everyInputDefined (f_states automaton)
  where
    everyInputDefined :: State -> Bool
    everyInputDefined state = all (hasLetter state) (f_alphabet automaton)
    hasLetter :: State -> Char -> Bool
    hasLetter state letter = letter `List.elem` (transitionLetters state)
    transitionLetters :: State -> [Char]
    transitionLetters state = map f_inputLetter (f_transitions state)


isNFA :: Automaton -> Bool
isNFA = not . isDFA


-- -------------------------------------------------------------------
-- Convert NFA to DFA
-- -------------------------------------------------------------------

type ConvID = [StateID]
data ConvState = ConvState { f_convID :: ConvID
                           , f_convIsInitial :: Bool
                           , f_convIsAccepting :: Bool
                           , f_convTransitions :: [ConvTransition]
                           } deriving (Eq, Show)

data ConvTransition = ConvTransition { f_convFromState :: ConvID
                                     , f_convInputLetter :: Char
                                     , f_convToState :: ConvID
                                     } deriving (Eq, Show)

toDFA :: Automaton -> DFA
toDFA automaton
  | isDFA automaton   = automaton
  | otherwise         = Automaton { f_alphabet = f_alphabet automaton
                                  , f_states = convertBackStates
                                           . removeUselessStates
                                           . addTransitionFunctions originalAlphabet originalStates
                                           . addAcceptingStates originalStates
                                           . addInitialState originalStates
                                           . initConvStates
                                           $ originalStates
                                  }
  where
    originalAlphabet = f_alphabet automaton
    originalStates = f_states automaton


initConvStates :: [State] -> [ConvState]
initConvStates = map initState . powerset . map f_stateID
  where
    initState :: [StateID] -> ConvState
    initState stateIDs = ConvState { f_convID = stateIDs
                                   , f_convIsInitial = False
                                   , f_convIsAccepting = False
                                   , f_convTransitions = [] }
    powerset :: [a] -> [[a]]
    powerset = Monad.filterM (const [True, False])


addInitialState :: [State] -> [ConvState] -> [ConvState]
addInitialState states = map (\convState -> if ((f_convID convState) == initialStateID)
                                            then convState { f_convIsInitial = True }
                                            else convState)
  where
    initialStateID :: ConvID
    initialStateID = case getInitialState states of
                        Just i  -> [f_stateID i]
                        Nothing -> [-1]


addAcceptingStates :: [State] -> [ConvState] -> [ConvState]
addAcceptingStates states = map (\convState -> if not . null . acceptingIntersection $ convState
                                               then convState { f_convIsAccepting = True }
                                               else convState)
  where
    acceptingIntersection :: ConvState -> ConvID
    acceptingIntersection convState = (f_convID convState) `List.intersect` acceptingStateIDs
    acceptingStateIDs :: [StateID]
    acceptingStateIDs = map f_stateID (getAcceptingStates states)


addTransitionFunctions :: Alphabet -> [State] -> [ConvState] -> [ConvState]
addTransitionFunctions alphabet states convStates = map addConvTransitions convStates
  where
    addConvTransitions :: ConvState -> ConvState
    addConvTransitions convState = convState { f_convTransitions = findConvTransitions convState }
    findConvTransitions :: ConvState -> [ConvTransition]
    findConvTransitions convState = concat $ map (findConvTransitionsWithLetter convState) alphabet
    findConvTransitionsWithLetter :: ConvState -> Char -> [ConvTransition]
    findConvTransitionsWithLetter convState char
       | null (getTransitionsForLetterAndState char convState)  = [createTransition convState char emptyConvState]
       | otherwise                                              = [createTransition convState char (findAccompanyingState convState char)]
    createTransition :: ConvState -> Char -> ConvState -> ConvTransition
    createTransition from letter to = ConvTransition { f_convFromState = f_convID from
                                                     , f_convInputLetter = letter
                                                     , f_convToState = f_convID to }
    reduceFromStateToID :: [Transition] -> ConvID
    reduceFromStateToID = List.sort . map f_toState
    getTransitionsForLetterAndState :: Char -> ConvState -> [Transition]
    getTransitionsForLetterAndState char convState = filter (\transition -> (f_fromState transition) `elem` (f_convID convState))
                                                            (getTransitionsForLetter char)
    getTransitionsForLetter :: Char -> [Transition]
    getTransitionsForLetter char = filter (\transition ->  (f_inputLetter transition) == char) (getTransitions states)
    emptyConvState :: ConvState
    emptyConvState = case (getNullState convStates) of
                        Just x  -> x
                        Nothing -> failState
    findAccompanyingState :: ConvState -> Char -> ConvState
    findAccompanyingState source char = case (getConvState (reduceFromStateToID (getTransitionsForLetterAndState char source)) convStates) of
                                            Just x -> x
                                            Nothing -> failState
    failState :: ConvState
    failState = ConvState {f_convID=[-1], f_convIsInitial=False, f_convIsAccepting=False, f_convTransitions=[]}


removeUselessStates :: [ConvState] -> [ConvState]
removeUselessStates convStates = filter (\cs -> f_convIsInitial cs || isReachable cs) convStates
  where
    isReachable :: ConvState -> Bool
    isReachable convState = f_convID convState `elem` allToStates
    allToStates :: [ConvID]
    allToStates = map f_convToState allTransitions
    allTransitions :: [ConvTransition]
    allTransitions = foldl (++) [] . map f_convTransitions $ convStates


convertBackStates :: [ConvState] -> [State]
convertBackStates allConvStates = map convertState allConvStates
  where
    convertState :: ConvState -> State
    convertState convState = State { f_stateID =  convertID (f_convID convState)
                                   , f_isInitial = f_convIsInitial convState
                                   , f_isAccepting = f_convIsAccepting convState
                                   , f_transitions = map convertTransition (f_convTransitions convState) }
    convertTransition :: ConvTransition -> Transition
    convertTransition convTransition = Transition { f_fromState = convertID (f_convFromState convTransition)
                                                  , f_inputLetter = f_convInputLetter convTransition
                                                  , f_toState = convertID (f_convToState convTransition) }
    convertID :: ConvID -> StateID
    convertID convID = Maybe.fromMaybe (-1) (lookup convID incrementedStateIDs)
    incrementedStateIDs :: [(ConvID, StateID)]
    incrementedStateIDs = snd $ List.mapAccumL(\index conv -> (index + 1, (conv, index))) 0 allConvIDs
    allConvIDs :: [ConvID]
    allConvIDs = List.sortBy (Monoid.mconcat [Ord.comparing length, compare])
               . map f_convID $ allConvStates

getNullState :: [ConvState] -> Maybe ConvState
getNullState = List.find (\state -> f_convID state == [])

getConvState :: ConvID -> [ConvState] -> Maybe ConvState
getConvState targetID = List.find (\state -> f_convID state == targetID)


-- -------------------------------------------------------------------
-- Modify DFA
-- -------------------------------------------------------------------

complement :: DFA -> DFA
complement dfa = dfa { f_states = map invertAccept (f_states dfa) }
  where
    invertAccept :: State -> State
    invertAccept state = state { f_isAccepting = not (f_isAccepting state)}


intersection :: DFA -> DFA -> DFA
intersection = undefined
