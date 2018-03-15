module Main where

import Parser

main :: IO ()
main = do
  contents <- readFile "input.txt"
  print . parseProgram $ (lines contents)
