module Main where

import qualified Data.Maybe         as Maybe
import qualified System.Environment as Environment

import qualified Parser
import qualified FiniteAutomata
import qualified Output
import qualified ShortestString


main :: IO ()
main = do
  args <- Environment.getArgs
  contents <- readFile (head args)
  let automaton = Maybe.fromJust . Parser.parseProgram $ (lines contents)
  let dfa = FiniteAutomata.toDFA automaton
  let complement = FiniteAutomata.complement automaton
  writeFile "output/1208487250_Milestone1_Dp.txt"
        . unlines
        . Output.automaton
        $ (complement)
  writeFile "output/1208487250_Milestone1_str.txt"
        . ShortestString.shortest
        $ (complement)
