module Main where

import qualified Data.Maybe         as Maybe
import qualified System.Directory   as Directory
import qualified System.Environment as Environment
import qualified System.Exit        as Exit

import qualified Parser
import qualified DFA
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
  let specDFA = DFA.toDFA . Maybe.fromJust $ spec
  let systemDFA = DFA.toDFA . Maybe.fromJust $ system
  -- Complement & intersection
  let specComplement = DFA.complement specDFA
  let intersection = Intersection.intersection specComplement systemDFA
  let string = ShortestString.shortest intersection
  let consoleOutput = if null string
                      then "The system satisfies the specification!"
                      else "The system does not satisfy the specification! Counterexample: " ++ string
  print consoleOutput
  Directory.createDirectoryIfMissing False "output"
  writeFile "output/1208487250_Final_Dp.txt" (unlines (Output.automaton intersection))
  writeFile "output/1208487250_Final_str.txt" string


getFileName :: IO String
getFileName = do
    args <- Environment.getArgs
    let fileName = case args of
                     [] -> "input/final_pump-s1-fixed.txt"
                     x:_ -> x
    return fileName


exitIfParseFail :: Maybe a -> String -> IO ()
exitIfParseFail (Just _) _ = return ()
exitIfParseFail Nothing target = do print ("Invalid input for " ++ target ++ ".")
                                    Exit.exitFailure
