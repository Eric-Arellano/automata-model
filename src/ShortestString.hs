module ShortestString (shortest) where

-- BFS algorithm adapted from https://lettier.github.io/posts/2016-04-29-breadth-first-search-in-haskell.html

import qualified Data.List as List

import qualified FiniteAutomata as FA


shortest :: FA.Automaton -> String
shortest automaton = accompanyingString automaton
                   . shortestAcceptingVertex
                   . addDistances (initialVertex automaton)
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
toVertex state = Vertex { stateID = FA.number state
                        , neighbors = map FA.number . map FA.toState $ FA.transitions state
                        , isAccepting = FA.isAccepting state
                        }

initialVertex :: FA.Automaton -> Vertex
initialVertex = toVertex . FA.getInitialState . FA.states

-- -------------------------------------------------
-- BFS
-- -------------------------------------------------

addDistances :: Vertex -> Graph -> Graph
addDistances = undefined

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