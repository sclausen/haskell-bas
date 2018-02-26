{-# LANGUAGE OverloadedStrings #-}
module Main where

import           AnsiText
import           Data.Storage.Storage
import           Data.Version
import           Paths_haskell_bas        (version)
import           Repl
import           System.Console.Haskeline

main :: IO ()
main = do
  putStrLn $ makeAnsiText $ "bas " ++ showVersion version
  storage <- newStorage
  settings <- makeSettings
  runInputT settings (repl storage)
