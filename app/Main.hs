{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Repl
import           System.Console.Haskeline

main :: IO ()
main = runInputT appSettings repl
