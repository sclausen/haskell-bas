module Main where

import           Data.Storage.Storage
import           Repl
import           System.Console.Haskeline

main :: IO ()
main = do
  storage <- newStorage
  runInputT appSettings (repl storage)
