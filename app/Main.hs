module Main where

import Lib
import Room1

main :: IO ()
main = do
  contents <- readFile "inputs/r1p1.txt"
  putStr $ puzzle1R contents
  putStr "\n"
  putStr $ puzzle1L contents
  putStr "\n"
