module ShortestString (shortest, findReachable) where

-- BFS algorithm adapted from https://lettier.github.io/posts/2016-04-29-breadth-first-search-in-haskell.html

import qualified Data.List  as List
import qualified Data.Maybe as Maybe

import qualified FiniteAutomata as FA


shortest :: FA.Automaton -> String
shortest automaton = case (shortestAcceptingVertex automaton graph) of
                        Just vertex -> accompanyingString automaton graph vertex
                        Nothing -> ""
  where
    graph = toGraphWithDistances automaton


findReachable :: FA.Automaton -> [FA.StateID]
findReachable automaton = map f_stateID (x:xs)
  where
    (Graph (x:xs)) = graph
    graph :: Graph
    graph = toGraphWithDistances automaton


-- -------------------------------------------------
-- Graph data structure
-- -------------------------------------------------

data Vertex = Vertex { f_stateID :: FA.StateID
                     , f_neighbors :: [FA.StateID]
                     , f_parent :: FA.StateID
                     , f_distance :: Int
                     } deriving (Show, Eq)

data Graph = Graph [Vertex] deriving (Show, Eq)

toGraph :: FA.Automaton -> Graph
toGraph automaton = Graph (map (toVertex automaton) (FA.f_states automaton))

toVertex :: FA.Automaton -> FA.StateID -> Vertex
toVertex automaton stateID = Vertex { f_stateID = stateID
                                    , f_neighbors = findNeighbors
                                    , f_distance = 0
                                    , f_parent = -1 }
  where
    findNeighbors = map FA.f_toState
                  . filter (\t -> (FA.f_fromState t) == stateID)
                  $ FA.f_transitions automaton

findVertex :: Graph -> FA.StateID -> Vertex
findVertex (Graph vertexes) targetID = Maybe.fromJust . List.find (\vertex -> f_stateID vertex == targetID) $ vertexes


-- -------------------------------------------------
-- BFS
-- -------------------------------------------------

toGraphWithDistances :: FA.Automaton -> Graph
toGraphWithDistances automaton = addDistances (findVertex emptyGraph (FA.f_initialState automaton)) emptyGraph
  where
    emptyGraph = toGraph automaton

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
  -- Only returns Vertexes reachable from StartVertex.
  where
    currentID :: FA.StateID
    currentID = f_stateID current
    currentNeighborsIDs :: [FA.StateID]
    currentNeighborsIDs = f_neighbors current
    currentNeighborsVertexes :: [Vertex]
    currentNeighborsVertexes = getVertexesForIDs inGraph currentNeighborsIDs
    updatedDistance :: Int
    updatedDistance = f_distance current + 1
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
getVertexesForIDs (Graph vertexes) ids = filter (\vertex -> f_stateID vertex `elem` ids) vertexes

vertexInVertexes :: Vertex -> [Vertex] -> Bool
vertexInVertexes state vertexes = (f_stateID state) `elem` (map f_stateID vertexes)

filterNeighbors :: [Vertex] -> [Vertex] -> [Vertex]
filterNeighbors _ [] = []
filterNeighbors [] _ = []
filterNeighbors seen vertexNeighbors = filter (\vertex -> not $ vertexInVertexes vertex seen) vertexNeighbors

updateDistanceParent :: [Vertex] -> Int -> FA.StateID -> [Vertex]
updateDistanceParent [] _ _ = []
updateDistanceParent vertexes distance parent = map (\vertex -> vertex {f_distance = distance, f_parent = parent}) vertexes


-- -------------------------------------------------
-- Shortest path
-- -------------------------------------------------

shortestAcceptingVertex :: FA.Automaton -> Graph -> Maybe Vertex
shortestAcceptingVertex automaton (Graph vertexes) = List.find (\vertex -> (f_stateID vertex) `elem` (FA.f_acceptingStates automaton))
                                                   . List.sortBy compareVertexDistance
                                                   $ vertexes
  where
    compareVertexDistance :: Vertex -> Vertex -> Ordering
    compareVertexDistance v1 v2 = compare (f_distance v1) (f_distance v2)


-- -------------------------------------------------
-- Accompanying string
-- -------------------------------------------------

accompanyingString :: FA.Automaton -> Graph -> Vertex -> String
accompanyingString automaton graph final = case getTransitionLetters automaton graph final [] of
                                             (x:xs) -> x : xs
                                             []    -> "epsilon"

getTransitionLetters :: FA.Automaton -> Graph -> Vertex -> [Char] -> [Char]
getTransitionLetters automaton graph vertex priorLetters
  | (f_stateID vertex) == (FA.f_initialState automaton) = priorLetters
  | otherwise                                           = getTransitionLetters automaton graph parentVertex updatedLetters
  where
    parentVertex :: Vertex
    parentVertex = findVertex graph (f_parent vertex)
    updatedLetters :: [Char]
    updatedLetters = findChar : priorLetters  -- prepend new letter
    findChar :: Char
    findChar = FA.f_inputLetter findTransition
    findTransition :: FA.Transition
    findTransition = head
                   . filter (\transition ->  FA.f_fromState transition == f_stateID parentVertex
                              && FA.f_toState transition == f_stateID vertex)
                   $ FA.f_transitions automaton
