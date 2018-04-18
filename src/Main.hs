module Main where

import qualified Data.Maybe         as Maybe
import qualified System.Environment as Environment
import qualified System.Exit        as Exit

import qualified Parser
import qualified FiniteAutomata
import qualified Output
import qualified ShortestString


main :: IO ()
main = do
  -- Read input
  fileName <- getFileName
  contents <- readFile fileName
  -- Parse
  let spec = Parser.parseSpecification (lines contents)
  exitIfParseFail spec "specification automaton"
  let system = Parser.parseSystem (lines contents)
  exitIfParseFail spec "system automaton"
  -- Convert to DFA
  let specDFA = FiniteAutomata.toDFA . Maybe.fromJust $ spec
  print specDFA
  let systemDFA = FiniteAutomata.toDFA . Maybe.fromJust $ system
  -- Complement & intersection
  let specComplement = FiniteAutomata.complement specDFA
--  let intersection = FiniteAutomata.intersection specComplement system
  let string = ShortestString.shortest specDFA
--  let string = ShortestString.shortest specComplement
--  let string = ShortestString.shortest intersection
  let consoleOutput = if null string then "Accepted" else string
--  print consoleOutput
  writeFile "output/1208487250_Milestone2_Dp.txt"
        . unlines
        . Output.automaton
        $ (specDFA)
----        $ (specComplement)
----        $ (intersection)
--  writeFile "output/1208487250_Milestone2_str.txt" string
  print "testing"


getFileName :: IO String
getFileName = do
    args <- Environment.getArgs
    let fileName = case args of
                     [] -> "input/m2_basic.txt"
--                     [] -> "input/m1_nfa-simple.txt"
                     x:_ -> x
    return fileName


exitIfParseFail :: Maybe a -> String -> IO ()
exitIfParseFail (Just _) _ = return ()
exitIfParseFail Nothing target = do print ("Invalid input for " ++ target ++ ".")
                                    Exit.exitFailure
