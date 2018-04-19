module Main where

import qualified Data.Maybe         as Maybe
import qualified System.Environment as Environment
import qualified System.Exit        as Exit

import qualified Parser
import qualified FiniteAutomata
import qualified Intersection
import qualified Output
import qualified ShortestString


main :: IO ()
main = do
  -- Read input
  fileName <- getFileName
  contents <- readFile fileName
  -- Parse
  let (spec, system) = Parser.parseAutomata (lines contents)
  exitIfParseFail spec "specification automaton"
  exitIfParseFail system "system automaton"
  -- Convert to DFA
  let specDFA = FiniteAutomata.toDFA . Maybe.fromJust $ spec
  let systemDFA = FiniteAutomata.toDFA . Maybe.fromJust $ system
  -- Complement & intersection
--  let specComplement = FiniteAutomata.complement specDFA
--  let intersection = Intersection.intersection specComplementDFA systemDFA
  let intersection = Intersection.intersection (Maybe.fromJust spec) (Maybe.fromJust system)
  print intersection
--  let string = ShortestString.shortest specDFA
--  let string = ShortestString.shortest systemDFA
--  let string = ShortestString.shortest specComplement
  let string = ShortestString.shortest intersection
  let consoleOutput = if null string then "Accepted" else string
  print consoleOutput
  writeFile "output/1208487250_Milestone2_Dp.txt" (unlines (Output.automata specDFA systemDFA))
--  writeFile "output/1208487250_Milestone2_Dp.txt" (unlines (Output.automata (Maybe.fromJust spec) (Maybe.fromJust system)))
  writeFile "output/1208487250_Milestone2_str.txt" string


getFileName :: IO String
getFileName = do
    args <- Environment.getArgs
    let fileName = case args of
                     [] -> "input/m2_intersection.txt"
--                     [] -> "input/m1_nfa-simple.txt"
                     x:_ -> x
    return fileName


exitIfParseFail :: Maybe a -> String -> IO ()
exitIfParseFail (Just _) _ = return ()
exitIfParseFail Nothing target = do print ("Invalid input for " ++ target ++ ".")
                                    Exit.exitFailure
