module Main where

import qualified Data.Maybe       as Maybe

import qualified Parser
import qualified FiniteAutomata
import qualified Output


main :: IO ()
main = do
  contents <- readFile "simple.txt"
  writeFile "output.txt"
        . unlines
        . Output.automaton
        . FiniteAutomata.complement
        . Maybe.fromJust
        . Parser.parseProgram
        $ (lines contents)
