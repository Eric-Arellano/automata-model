module Main where

import Parser

main :: IO ()
main = do
  contents <- readFile "input.txt"
  let programData = parseProgram (lines contents)
  print programData
