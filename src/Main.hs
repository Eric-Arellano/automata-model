module Main where

import qualified Data.Maybe       as Maybe

import qualified Parser
import qualified FiniteAutomata
import qualified Output
import qualified ShortestString


main :: IO ()
main = do
  contents <- readFile "input/nfa-simple.txt"
  let automaton = Maybe.fromJust . Parser.parseProgram $ (lines contents)
  let dfa = FiniteAutomata.toDFA automaton
--  let complement = FiniteAutomata.complement automaton
--  print . ShortestString.experiment $ automaton
  writeFile "output/1208487250_Milestone1_Dp.txt"
        . unlines
        . Output.automaton
        $ (dfa)
--  writeFile "1208487250_Milestone1_str.txt"
--        . ShortestString.shortest
--        $ (dfa)
