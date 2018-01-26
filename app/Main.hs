{-# LANGUAGE OverloadedStrings #-}
module Main where


import           Control.Concurrent       (threadDelay)
import           Control.Concurrent.MVar
import           Control.Monad            (forever, unless)
import           Control.Monad.Trans
import qualified Data.ByteString.Char8    as B8
import qualified Data.Yaml                as Y
import           Repl
import           Stock
import           System.Console.Haskeline
import           System.FilePath.Posix    (takeFileName)
import           System.FSNotify

-- runInputT :: Settings IO -> InputT IO a -> IO a
-- getInputLine :: String -> InputT IO (Maybe String)



--main = runInputT appSettings repl

main :: IO ()
main = do
  -- stock <- newEmptyMVar
  stock <- newMVar =<< readStock
  withManager (\mgr -> do
    watchDir mgr "." (\e-> "stock.yml" == takeFileName (eventPath e)) (\e-> do
      print e
      stock' <- readStock
      modifyMVar_ stock (\_-> pure stock')
      )
      -- sleep forever (until interrupted)
    runInputT appSettings (repl stock)
    -- forever $ do
    --   threadDelay 1000000
    --   stockDoesNotExist <- isEmptyMVar stock
    --   unless stockDoesNotExist (print =<< readMVar stock)
    )
