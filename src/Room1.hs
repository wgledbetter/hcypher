-- Room 1 is about steganography or "hidden" messages.
-- Some of the puzzles rely on visual effects or actually looking at the correct physical location (there's one around the back of the terminal).
-- Those aren't code-able, but I'll write methods for the ones that can be algorithmatized.

module Room1 (puzzle1, split, collectFirstChars) where

--------------------------------------------------------------------------------
-- Puzzle 1 --------------------------------------------------------------------
--------------------------------------------------------------------------------

puzzle1 :: String -> String
puzzle1 = collectFirstChars . split '\n'

--------------------------------------------------------------------------------

splitFolderRight :: Char -> Char -> [String] -> [String]
splitFolderRight s x []
  | s == x = [""]
  | otherwise = [[x]]
splitFolderRight s x (l : ls)
  | s == x = "" : (l : ls) -- Split here by adding an empty string to the front of the list
  | otherwise = (x : l) : ls -- Add this character (x) to the front of the first list element

split :: Char -> String -> [String]
split _ [] = []
split c s = foldr (splitFolderRight c) [] s

--------------------------------------------------------------------------------

collectFirstFolder :: String -> String -> String
collectFirstFolder [] s = ' ' : s -- I'm assuming here that a blank line turns into a space character
collectFirstFolder (c1 : _) [] = [c1]
collectFirstFolder (c1 : _) s = c1 : s

collectFirstChars :: [String] -> String
collectFirstChars = foldr collectFirstFolder ""
