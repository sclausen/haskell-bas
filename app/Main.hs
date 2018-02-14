module Main where

import           Data.Storage.Storage
import           Repl
import           System.Console.Haskeline

main :: IO ()
main = do
  storage <- newStorage
  settings <- makeSettings
  putStrLn ""
  putStrLn " ██████╗  █████╗ ███████╗     ██╗    ██████╗"
  putStrLn " ██╔══██╗██╔══██╗██╔════╝    ███║   ██╔═████╗"
  putStrLn " ██████╔╝███████║███████╗    ╚██║   ██║██╔██║"
  putStrLn " ██╔══██╗██╔══██║╚════██║     ██║   ████╔╝██║"
  putStrLn " ██████╔╝██║  ██║███████║     ██║██╗╚██████╔╝"
  putStrLn " ╚═════╝ ╚═╝  ╚═╝╚══════╝     ╚═╝╚═╝ ╚═════╝"
  putStrLn ""
  runInputT settings (repl storage)
