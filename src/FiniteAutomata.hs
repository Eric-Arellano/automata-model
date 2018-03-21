module FiniteAutomata
  ( Automaton(..)
  , State(..)
  , Transition(..)
  , toDFA
  , isNFA
  , isDFA
  , complement
  ) where


import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Control.Monad as Monad

data Automaton = Automaton { alphabet :: Alphabet
                           , states :: [State]
                           } deriving (Show, Eq)

type Alphabet = [Char]

type StateID = Int
data State = State { number :: StateID
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
initConvStates = map initState . powerset . map number
  where
    initState :: [StateID] -> ConvState
    initState stateIDs = ConvState { convID = stateIDs
                                   , convIsInitial = False
                                   , convIsAccepting = False
                                   , convTransitions = [] }
    powerset :: [a] -> [[a]]
    powerset = Monad.filterM (const [True, False])


addInitialState :: [State] -> [ConvState] -> [ConvState]
addInitialState states = map (\convState -> if ((convID convState) == initialState)
                                            then convState { convIsInitial = True }
                                            else convState)
  where
    initialState :: [StateID]
    initialState = map number . filter (\state -> isInitial state == True) $ states


addAcceptingStates :: [State] -> [ConvState] -> [ConvState]
addAcceptingStates states = map (\convState -> if not . null . acceptingIntersection $ convState
                                               then convState { convIsAccepting = True }
                                               else convState)
  where
    acceptingIntersection :: ConvState -> [StateID]
    acceptingIntersection convState = (convID convState) `List.intersect` acceptingStates
    acceptingStates :: [StateID]
    acceptingStates = map number . filter (\state -> isAccepting state == True) $ states


addTransitionFunctions :: Alphabet -> [State] -> [ConvState] -> [ConvState]
addTransitionFunctions alphabet states = map addToState
  where
    addToState :: ConvState -> ConvState
    addToState convState = convState { convTransitions = convertForState convState }
    convertForState :: ConvState -> [ConvTransition]
    convertForState convState = undefined
    convertForLetterAndState :: ConvState -> Char -> [ConvTransition]
    convertForLetterAndState convState char = undefined
    allTransitionsForLetterAndState :: Char -> ConvState -> [Transition]
    allTransitionsForLetterAndState char convState = filter (\transition -> number (fromState transition) `elem` (convID convState)) (allTransitionsForLetter char)
    allTransitionsForLetter :: Char -> [Transition]
    allTransitionsForLetter char = filter (\transition ->  (inputLetter transition) == char) allTransitions
    allTransitions :: [Transition]
    allTransitions = foldl (++) [] . map transitions $ states

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
convertBackStates = map convertState
  where
    convertState :: ConvState -> State
    convertState convState = State { number = flattenID (convID convState)
                                   , isInitial = convIsInitial convState
                                   , isAccepting = convIsAccepting convState
                                   , transitions = map convertTransition (convTransitions convState) }
    convertTransition :: ConvTransition -> Transition
    convertTransition convTransition = Transition { fromState = convertState (convFromState convTransition)
                                                  , inputLetter = convInputLetter convTransition
                                                  , toState = convertState (convToState convTransition) }
    flattenID :: [StateID] -> StateID
    flattenID = foldl ((+).(*10)) 0


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
