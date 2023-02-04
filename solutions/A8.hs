module A8 where

import Provided
import A6 hiding ( validateNoDict, validateWithDict )
import A7

import Control.Monad
import Control.Monad.State

-- *** A8: Monads *** --

-- Q#01

validateNoDict :: Secret -> Either GameException Secret
validateNoDict s = hasValidChars s >>= isValidLength

validateWithDict :: Dictionary -> Secret -> Either GameException Secret
validateWithDict d s= validateNoDict s >>= isInDict d

-- Q#02

playGame :: Game -> IO ()
playGame g = do
    promptGuess
    c <- getUpperChar
    _SPACE_
    case processTurn c g of
        Left GameOver -> print GameOver >> putStrLn ("Correct Word: " ++ show (secret g)) >> return ()
        Left e -> print e >> playGame g
        Right g2 ->
            print g2 >>
                if secret g2 == guess g2
                    then putStrLn "You win!"
                    else playGame g2

-- Q#03

startGame :: (Secret -> Either GameException Secret) -> IO ()
startGame v = do
    s <- setSecret
    case makeGameIfValid (v s) of
        Left e -> print e >> startGame v
        Right g -> playGame g

-- Q#04

runApp :: IO ()
runApp = do
    d <- getDict
    case d of
        Nothing -> do
            putStrLn "Missing dictionary file! Continue without dictionary? [Y/N]"
            c <- getUpperChar
            when (c == 'Y') $ startGame validateNoDict
        Just d -> startGame (validateWithDict d)

-- Q#05

makeGameS = undefined


updateGameS = undefined


repeatedMoveS = undefined


processTurnS = undefined