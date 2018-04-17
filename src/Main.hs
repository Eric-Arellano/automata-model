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
  let fileName = if null args then "input/nfa-simple.txt" else head args
  contents <- readFile fileName
  let specification = FiniteAutomata.toDFA . Maybe.fromJust . Parser.parseSpecification $ (lines contents)
  let system = FiniteAutomata.toDFA . Maybe.fromJust . Parser.parseSystem $ (lines contents)
  let specComplement = FiniteAutomata.complement specification
--  let intersection = FiniteAutomata.intersection specComplement system
  let string = ShortestString.shortest specification
--  let string = ShortestString.shortest specComplement
--  let string = ShortestString.shortest intersection
  let consoleOutput = if null string then "Accepted" else string
  print consoleOutput
  writeFile "output/1208487250_Milestone2_Dp.txt"
        . unlines
        . Output.automaton
        $ (specification)
--        $ (specComplement)
--        $ (intersection)
  writeFile "output/1208487250_Milestone2_str.txt" string