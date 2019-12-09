module Main where

import Lib
import System.Environment

main :: IO ()
main = do
  [configName] <- getArgs
  recognizeImages configName signSelector parseSign
