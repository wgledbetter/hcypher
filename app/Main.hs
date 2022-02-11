module Main where

import Lib
import Room1
import Room2

main :: IO ()
main = do
  contents <- readFile "inputs/r2p2.txt"
  putStr $ Room2.puzzle2R contents
  putStr "\n"
  putStr $ Room2.puzzle2L contents
  putStr "\n"
