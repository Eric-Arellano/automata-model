module Main where

import qualified Parser
import qualified FiniteAutomata
import qualified Data.Maybe       as Maybe

main :: IO ()
main = do
  contents <- readFile "input.txt"
  print . FiniteAutomata.output
        . Maybe.fromJust
        . Parser.parseProgram
        $ (lines contents)
