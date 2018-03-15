module Main where

import Parser

main :: IO ()
main = do
  contents <- readFile "hello.txt"
  let lns = lines contents
  let programData = parseProgram lns
  print programData
