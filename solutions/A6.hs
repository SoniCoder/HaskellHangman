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
data Game = Game {
    secret :: Secret
  , guess :: Guess
  , moves :: [Move]
  , chances :: Chances
  }

-- Q#09

repeatedMove :: Move -> Game -> Bool
repeatedMove m g =
  elem m $ moves g

-- Q#10

makeGame :: Secret -> Game
makeGame s =
  Game {
    secret = map toUpper s,
    guess = map (const '_') s,
    moves = [],
    chances = _CHANCES_
  }

-- Q#11

updateGame :: Move -> Game -> Game
updateGame m g =
  let updatedGuess = revealLetters m (secret g) (guess g)
    in g {
      guess = updatedGuess,
      moves = m : moves g,
      chances = updateChances m (secret g) (chances g)
    }

-- Q#12

showGameHelper :: String -> [Char] -> Int -> String
showGameHelper game moves chances = unlines [
      _STARS_
    , "\tSecret Word:\t" ++ intersperse ' ' game ++ "\n"
    , "\tGuessed:\t" ++ intersperse ' ' (sort moves) ++ "\n"
    , "\tChances:\t" ++ show chances
    , _STARS_
    ]

instance Show Game where
  show g = showGameHelper (guess g) (moves g) (chances g)

-- Q#13
instance Show GameException where
  show InvalidWord = concat ["Invalid word, length must be between ", lb, " and ", " characters."]
        where
            lb = show $ fst _LENGTH_
            ub = show $ snd _LENGTH_
  show InvalidMove = "Invalid move"
  show RepeatMove = "Repeat move"
  show GameOver = "Game over"

-- *** A6-2: Exception Contexts *** --

-- Q#14

toMaybe :: Bool -> a -> Maybe a
toMaybe False x = Nothing
toMaybe True x = Just x

-- Q#15

validateSecret :: (Secret -> Bool) -> Secret -> Either GameException Secret
validateSecret p s = if p s then Right s else Left InvalidWord

-- Q#16

hasValidChars :: Secret -> Either GameException Secret
hasValidChars s = validateSecret (all isAlpha) s


isValidLength :: Secret -> Either GameException Secret
isValidLength s = validateSecret lengthInRange s


isInDict :: Dictionary -> Secret -> Either GameException Secret
isInDict d s = validateSecret (\s0 -> elem (map toLower s0) d) s

-- Q#17

validateNoDict :: Secret -> Either GameException Secret
validateNoDict s = case hasValidChars s of
  Left e -> Left e
  Right s -> isValidLength s

validateWithDict :: Dictionary -> Secret -> Either GameException Secret
validateWithDict d s = case validateNoDict s of
  Left e -> Left e
  Right s -> isInDict d s

-- Q#18

processTurn :: Move -> Game -> Either GameException Game
processTurn m g
  | invalidMove m = Left InvalidMove
  | repeatedMove m g = Left RepeatMove
  | chances updatedGame == 0 = Left GameOver
  | otherwise = Right updatedGame
    where updatedGame = updateGame m g