module Main where

import qualified Data.Maybe       as Maybe

import qualified Parser
import qualified FiniteAutomata
import qualified Output
import qualified ShortestString


main :: IO ()
main = do
  contents <- readFile "input2.txt"
  let automaton = Maybe.fromJust . Parser.parseProgram $ (lines contents)
  let complement = FiniteAutomata.complement automaton
  print . ShortestString.experiment $ automaton
--  writeFile "output.txt"
--        . unlines
--        . Output.automaton
--        $ (complement)
