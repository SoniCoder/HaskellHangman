module Main where

import System.IO

import A8
import Provided

main :: IO ()
main = do
  -- disable line buffering to prevent print issues in compiled executable:
  hSetBuffering stdout NoBuffering
  hSetBuffering stdin  NoBuffering
  putStrLn _LOGO_

  runApp