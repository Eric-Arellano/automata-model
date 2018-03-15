module Main where

import qualified Parser

main :: IO ()
main = do
  contents <- readFile "input.txt"
  print . Parser.parseProgram $ (lines contents)
