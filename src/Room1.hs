-- Room 1 is about steganography or "hidden" messages.
-- Some of the puzzles rely on visual effects or actually looking at the correct physical location (there's one around the back of the terminal).
-- Those aren't code-able, but I'll write methods for the ones that can be algorithmatized.

module Room1 (puzzle1R, splitR, collectFirstCharsR, puzzle1L, splitL, collectFirstCharsL) where

import Data.List.NonEmpty
import qualified Data.List.NonEmpty as GHC.Base

--------------------------------------------------------------------------------
-- Utilities -------------------------------------------------------------------
--------------------------------------------------------------------------------

morse :: GHC.Base.NonEmpty ([Char], Char)
morse =
  fromList
    [ (".-", 'A'),
      ("-...", 'B'),
      ("-.-.", 'C'),
      ("-..", 'D'),
      (".", 'E'),
      ("..-.", 'F'),
      ("--.", 'G'),
      ("....", 'H'),
      ("..", 'I'),
      (".---", 'J'),
      ("-.-", 'K'),
      (".-..", 'L'),
      ("--", 'M'),
      ("-.", 'N'),
      ("---", 'O'),
      (".--.", 'P'),
      ("--.-", 'Q'),
      (".-.", 'R'),
      ("...", 'S'),
      ("-", 'T'),
      ("..-", 'U'),
      ("...-", 'V'),
      (".--", 'W'),
      ("-..-", 'X'),
      ("-.--", 'Y'),
      ("--..", 'Z'),
      (".----", '1'),
      ("..---", '2'),
      ("...--", '3'),
      ("....-", '4'),
      (".....", '5'),
      ("-....", '6'),
      ("--...", '7'),
      ("---..", '8'),
      ("----.", '9'),
      ("----", '0')
    ]

--------------------------------------------------------------------------------
-- Puzzle 1 --------------------------------------------------------------------
--------------------------------------------------------------------------------

puzzle1R :: String -> String
puzzle1R = collectFirstCharsR . splitR '\n'

puzzle1L :: String -> String
puzzle1L = collectFirstCharsL . splitL '\n'

--------------------------------------------------------------------------------

splitFolderRight :: Char -> Char -> [String] -> [String]
splitFolderRight s x []
  | s == x = [""]
  | otherwise = [[x]]
splitFolderRight s x (l : ls)
  | s == x = "" : (l : ls) -- Split here by adding an empty string to the front of the list
  | otherwise = (x : l) : ls -- Add this character (x) to the front of the first list element

splitR :: Char -> String -> [String]
splitR _ [] = []
splitR c s = foldr (splitFolderRight c) [] s

--------------------------------------------------------------------------------

collectFirstFolderR :: String -> String -> String
collectFirstFolderR [] s = ' ' : s -- I'm assuming here that a blank line turns into a space character
collectFirstFolderR (c1 : _) [] = [c1]
collectFirstFolderR (c1 : _) s = c1 : s

collectFirstCharsR :: [String] -> String
collectFirstCharsR = foldr collectFirstFolderR ""

--------------------------------------------------------------------------------

splitFolderLeft :: Char -> [String] -> Char -> [String]
splitFolderLeft s [] x
  | s == x = [""]
  | otherwise = [[x]]
splitFolderLeft s ll x
  | s == x = ll ++ [""]
  | otherwise = init ll ++ [last ll ++ [x]]

splitL :: Char -> String -> [String]
splitL _ [] = []
splitL c s = foldl (splitFolderLeft c) [] s

--------------------------------------------------------------------------------

collectFirstFolderL :: String -> String -> String
collectFirstFolderL s [] = s ++ " "
collectFirstFolderL [] (c1 : _) = [c1]
collectFirstFolderL s (c1 : _) = s ++ [c1]

collectFirstCharsL :: [String] -> String
collectFirstCharsL = foldl collectFirstFolderL ""

--------------------------------------------------------------------------------
-- Puzzle 2 --------------------------------------------------------------------
--------------------------------------------------------------------------------
-- Notes: Puzzle 2 requires you to identify misspellings and collect the missing or added letters.

--------------------------------------------------------------------------------
-- Puzzle 3 --------------------------------------------------------------------
--------------------------------------------------------------------------------
-- Notes: Invisible Ink. Not codeable.

--------------------------------------------------------------------------------
-- Puzzle 4 --------------------------------------------------------------------
--------------------------------------------------------------------------------
-- Notes: Identify italics

--------------------------------------------------------------------------------
-- Puzzle 5 --------------------------------------------------------------------
--------------------------------------------------------------------------------
-- Notes: Very small font size. Not codeable.

--------------------------------------------------------------------------------
-- Puzzle 6 --------------------------------------------------------------------
--------------------------------------------------------------------------------
-- Notes: Convert ##:## ##:## into letters with A = 00, B = 01, etc.
