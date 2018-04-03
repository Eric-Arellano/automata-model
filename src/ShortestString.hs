module ShortestString (shortest, experiment) where

-- BFS algorithm adapted from https://lettier.github.io/posts/2016-04-29-breadth-first-search-in-haskell.html

import qualified Data.List as List

import qualified FiniteAutomata as FA


shortest :: FA.Automaton -> String
shortest automaton = accompanyingString automaton
                   . shortestAcceptingVertex
                   . addDistances (initialVertex automaton)
                   . toGraph $ automaton

experiment :: FA.Automaton -> Graph
experiment automaton = addDistances (initialVertex automaton)
                     . toGraph $ automaton

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
bfs inGraph (Graph (outStart:outEnd)) (current:remainingQueue) seenAlready = bfs inGraph outGraph updatedQueue updatedSeenAlready
  where
    currentID :: FA.StateID
    currentID = stateID current
    currentNeighborsIDs :: [FA.StateID]
    currentNeighborsIDs = neighbors current
    currentNeighborsVertexes :: [Vertex]
    currentNeighborsVertexes = getVertexesForIDs inGraph currentNeighborsIDs
    updatedDistance :: Int
    updatedDistance = distance current + 1
    -- Remove all neighbors that have been queued up before
    neighborsNotAlreadySeen = filterNeighbors seenAlready currentNeighborsVertexes
    -- Update the predecessor label and distance for each current vertex neighbor.
    enqueue :: [Vertex]
    enqueue = updateDistanceParent neighborsNotAlreadySeen updatedDistance currentID
    -- Update breadth-first search tree/graph.
    outGraph :: Graph
    outGraph = Graph $ (outStart:outEnd) ++ enqueue
    updatedQueue :: [Vertex]
    updatedQueue = remainingQueue ++ enqueue
    updatedSeenAlready :: [Vertex]
    updatedSeenAlready = seenAlready ++ enqueue

getVertexesForIDs :: Graph -> [FA.StateID] -> [Vertex]
getVertexesForIDs (Graph []) _ = []  -- empty graph
getVertexesForIDs (Graph (x:y)) [] = x : y
getVertexesForIDs (Graph (x:y)) ids = filter (\vertex -> stateID vertex `elem` ids) (x:y)

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

-- TODO: return MAYBE?
shortestAcceptingVertex :: Graph -> Vertex
shortestAcceptingVertex (Graph (x:y)) = head
                                      . List.sortBy compareVertexDistance
                                      . filter (\vertex -> isAccepting vertex == True) $ (x:y)
  where
    compareVertexDistance :: Vertex -> Vertex -> Ordering
    compareVertexDistance v1 v2 = compare (distance v1) (distance v2)

-- -------------------------------------------------
-- Accompanying string
-- -------------------------------------------------

accompanyingString :: FA.Automaton -> Vertex -> String
accompanyingString automaton final = undefined