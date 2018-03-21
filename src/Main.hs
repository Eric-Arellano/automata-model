module Main where

import qualified Data.Maybe       as Maybe

import qualified Parser
import qualified FiniteAutomata
import qualified Output


main :: IO ()
main = do
  contents <- readFile "simple.txt"
  let automaton = Maybe.fromJust . Parser.parseProgram $ (lines contents)
  let complement = FiniteAutomata.complement automaton
  writeFile "output.txt"
        . unlines
        . Output.automaton
        $ (complement)
