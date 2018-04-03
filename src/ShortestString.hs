module ShortestString (shortest) where

-- BFS algorithm adapted from https://lettier.github.io/posts/2016-04-29-breadth-first-search-in-haskell.html

import qualified Data.List as List

import qualified FiniteAutomata as FA


shortest :: FA.Automaton -> String
shortest automaton = case (shortestAcceptingVertex graph) of
                        Just vertex -> accompanyingString automaton graph vertex
                        Nothing -> "Bad"
  where
    graph = addDistances (initialVertex automaton) . toGraph $ automaton


-- -------------------------------------------------
-- Graph data structure
-- -------------------------------------------------

data Vertex = Vertex { stateID :: FA.StateID
                     , neighbors :: [FA.StateID]
                     , parent :: FA.StateID
                     , distance :: Int
                     , isAccepting :: Bool
                     } deriving (Show, Eq)

data Graph = Graph [Vertex] deriving (Show, Eq)

toGraph :: FA.Automaton -> Graph
toGraph automaton = Graph (map toVertex (FA.states automaton))

toVertex :: FA.State -> Vertex
toVertex state = Vertex { stateID = FA.stateID state
                        , neighbors = map FA.stateID . map FA.toState $ FA.transitions state
                        , isAccepting = FA.isAccepting state
                        , distance = 0
                        , parent = -1
                        }

initialVertex :: FA.Automaton -> Vertex
initialVertex = toVertex . FA.getInitialState . FA.states

findState :: FA.Automaton -> Vertex -> FA.State
findState automaton vertex = head . filter (\state -> FA.stateID state == stateID vertex) $ FA.states automaton

findVertex :: Graph -> FA.StateID -> Vertex
findVertex (Graph vertexes) targetID = head . filter (\vertex -> stateID vertex == targetID) $ vertexes

-- -------------------------------------------------
-- BFS
-- -------------------------------------------------

addDistances :: Vertex -> Graph -> Graph
addDistances startVertex inGraph = bfs inGraph outGraph queue seen
  where
    queue = [startVertex]
    seen = [startVertex]
    outGraph = Graph queue

--     In       Out      Queue       Seen        Out
bfs :: Graph -> Graph -> [Vertex] -> [Vertex] -> Graph
bfs (Graph []) _ _ _ = Graph []  -- empty graph -> output empty graph
bfs _ outGraph [] _ = outGraph  -- empty queue -> output graph
bfs inGraph (Graph outVertexes) (current:remainingQueue) seenAlready = bfs inGraph outGraph updatedQueue updatedSeenAlready
  where
    currentID :: FA.StateID
    currentID = stateID current
    currentNeighborsIDs :: [FA.StateID]
    currentNeighborsIDs = neighbors current
    currentNeighborsVertexes :: [Vertex]
    currentNeighborsVertexes = getVertexesForIDs inGraph currentNeighborsIDs
    updatedDistance :: Int
    updatedDistance = distance current + 1
    -- Remove all neighbors already seen
    neighborsNotAlreadySeen = filterNeighbors seenAlready currentNeighborsVertexes
    -- Update each neighbor with parent & distance
    enqueue :: [Vertex]
    enqueue = updateDistanceParent neighborsNotAlreadySeen updatedDistance currentID
    -- Update graph
    outGraph :: Graph
    outGraph = Graph (outVertexes ++ enqueue)
    updatedQueue :: [Vertex]
    updatedQueue = remainingQueue ++ enqueue
    updatedSeenAlready :: [Vertex]
    updatedSeenAlready = seenAlready ++ enqueue

getVertexesForIDs :: Graph -> [FA.StateID] -> [Vertex]
getVertexesForIDs (Graph []) _ = []  -- empty graph
getVertexesForIDs (Graph vertexes) [] = vertexes
getVertexesForIDs (Graph vertexes) ids = filter (\vertex -> stateID vertex `elem` ids) vertexes

vertexInVertexes :: Vertex -> [Vertex] -> Bool
vertexInVertexes state vertexes = (stateID state) `elem` (map stateID vertexes)

filterNeighbors :: [Vertex] -> [Vertex] -> [Vertex]
filterNeighbors _ [] = []
filterNeighbors [] _ = []
filterNeighbors seen vertexNeighbors = filter (\vertex -> not $ vertexInVertexes vertex seen) vertexNeighbors

updateDistanceParent :: [Vertex] -> Int -> FA.StateID -> [Vertex]
updateDistanceParent [] _ _ = []
updateDistanceParent vertexes distance parentID = map (\vertex -> vertex {distance = distance, parent = parentID}) vertexes

-- -------------------------------------------------
-- Shortest path
-- -------------------------------------------------

shortestAcceptingVertex :: Graph -> Maybe Vertex
shortestAcceptingVertex (Graph vertexes) = List.find (\vertex -> isAccepting vertex == True)
                                         . List.sortBy compareVertexDistance
                                         $ vertexes
  where
    compareVertexDistance :: Vertex -> Vertex -> Ordering
    compareVertexDistance v1 v2 = compare (distance v1) (distance v2)


-- -------------------------------------------------
-- Accompanying string
-- -------------------------------------------------

accompanyingString :: FA.Automaton -> Graph -> Vertex -> String
accompanyingString automaton graph final
  | null string = "epsilon"
  | otherwise   = string
  where
    string = getTransitionLetters automaton graph final []

getTransitionLetters :: FA.Automaton -> Graph -> Vertex -> [Char] -> [Char]
getTransitionLetters automaton graph vertex priorLetters
  | FA.isInitial (findState automaton vertex) == True   = priorLetters
  | otherwise                                           = getTransitionLetters automaton graph parentVertex updatedLetters
  where
    parentVertex :: Vertex
    parentVertex = findVertex graph (parent vertex)
    updatedLetters :: [Char]
    updatedLetters = findChar : priorLetters  -- prepend new letter
    findChar :: Char
    findChar = FA.inputLetter findTransition
    findTransition :: FA.Transition
    findTransition = head
                   . filter (\transition -> FA.stateID (FA.fromState transition) == stateID parentVertex
                              && FA.stateID (FA.toState transition) == stateID vertex)
                   $ FA.getTransitions (FA.states automaton)