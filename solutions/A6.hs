{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use infix" #-}
module A6 where

import Provided

import Data.List ( intersperse, sort )
import Data.Char

-- *** A6-0: WARM-UP *** --

-- Q#01
type Chances = Int
type Guess = String
type Move = Char
type Secret = String
type Dictionary = [String]

-- Q#02
data GameException = InvalidWord | InvalidMove | RepeatMove | GameOver

-- Q#03

lengthInRange :: Secret -> Bool
lengthInRange s =
  let (x, y) = _LENGTH_ in
    length s >= x && length s <= y

-- Q#04

invalidMove :: Move -> Bool
invalidMove m =
  not $ isAlpha m

-- Q#05

revealLetters :: Move -> Secret -> Guess -> Guess
revealLetters m s g =
  zipWith f s g
  where
    f s_itr g_itr = if s_itr == m then s_itr else g_itr

-- Q#06

updateChances :: Move -> Secret -> Chances -> Chances
updateChances m s c =
  if elem m s then c else c - 1

-- Q#07

setSecret :: IO Secret
setSecret = do
  putStr "Enter a secret word:\t"
  showInput False
  s <- getLine
  showInput True
  _SPACE_
  return s


-- *** A6-1: Records & Instances *** --

-- Q#08
data Game

-- Q#09

repeatedMove = undefined

-- Q#10

makeGame = undefined

-- Q#11

updateGame = undefined

-- Q#12

showGameHelper :: String -> [Char] -> Int -> String
showGameHelper game moves chances = unlines [
      _STARS_
    , "\tSecret Word:\t" ++ intersperse ' ' game ++ "\n"
    , "\tGuessed:\t" ++ intersperse ' ' (sort moves) ++ "\n"
    , "\tChances:\t" ++ show chances
    , _STARS_
    ]


-- Q#13


-- *** A6-2: Exception Contexts *** --

-- Q#14

toMaybe = undefined

-- Q#15

validateSecret = undefined

-- Q#16

hasValidChars = undefined


isValidLength = undefined


isInDict = undefined

-- Q#17

validateNoDict = undefined

validateWithDict = undefined

-- Q#18

processTurn = undefined