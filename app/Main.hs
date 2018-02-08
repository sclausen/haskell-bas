module Main where

import           Data.Storage.Storage
import           Repl
import           System.Console.Haskeline

main :: IO ()
main = do
  storage <- newStorage
  settings <- makeSettings
  runInputT settings (repl storage)
