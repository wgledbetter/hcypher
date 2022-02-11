-- Room 2 is about transposition

module Room2
  ( puzzle2L,
    puzzle2R,
    puzzle2L',
    puzzle2R',
    transpose,
    groupFolderRight,
    groupIntoR,
    groupFolderLeft,
    groupIntoL,
  )
where

import Room1 (splitL, splitR)
import Prelude

--------------------------------------------------------------------------------
-- Puzzle 1 --------------------------------------------------------------------
--------------------------------------------------------------------------------
-- Notes:

--------------------------------------------------------------------------------
-- Puzzle 2 --------------------------------------------------------------------
--------------------------------------------------------------------------------
-- Notes: Takes 2 input strings and alternates letters.

-- Decoder using left fold
puzzle2L :: String -> String
puzzle2L = concat . transpose . splitL ' '

-- Decoder using right fold
puzzle2R :: String -> String
puzzle2R = concat . transpose . splitR ' '

-- Encoder (Assumes you already removed spaces from your input)
puzzle2L' :: Int -> String -> String
puzzle2L' n s = concat . transpose $ groupIntoL n (s ++ replicate m ' ')
  where
    m = (n - mod (length s) n) + n

puzzle2R' :: Int -> String -> String
puzzle2R' n s = concat . transpose $ groupIntoR n (s ++ replicate m ' ')
  where
    m = (n - mod (length s) n) + n

--------------------------------------------------------------------------------

transpose :: [[a]] -> [[a]]
transpose ([] : _) = []
transpose m = map head m : transpose (map tail m)

--------------------------------------------------------------------------------

groupFolderRight :: Int -> a -> [[a]] -> [[a]]
groupFolderRight _ x [] = [[x]]
groupFolderRight n x (l : ls)
  | n > length l = (x : l) : ls
  | otherwise = [x] : (l : ls)

groupIntoR :: Int -> [a] -> [[a]]
groupIntoR _ [] = []
groupIntoR n x = foldr (groupFolderRight n) [] x

--------------------------------------------------------------------------------

groupFolderLeft :: Int -> [[a]] -> a -> [[a]]
groupFolderLeft _ [] x = [[x]]
groupFolderLeft n ll x
  | n > length lll = init ll ++ [lll ++ [x]]
  | otherwise = ll ++ [[x]]
  where
    lll = last ll

groupIntoL :: Int -> [a] -> [[a]]
groupIntoL _ [] = []
groupIntoL n x = foldl (groupFolderLeft n) [] x

--------------------------------------------------------------------------------
-- Puzzle 3 --------------------------------------------------------------------
--------------------------------------------------------------------------------
-- Notes: Reverse
