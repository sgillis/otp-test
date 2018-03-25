module Main where

import Control.Concurrent (threadDelay)
import Control.Distributed.Process (say, liftIO)
import Control.Distributed.Process.Node (runProcess)
import Control.Monad (mapM_)
import Options.Applicative (execParser)

import ClusterSetup (createMaster, createServer, registerPeers)
import Config (parse)
import Options (Options(..), options)
import Server (sendStop, sendShutdown)

main :: IO ()
main = execParser options >>= main'

main' :: Options -> IO ()
main' options = do
  config <- parse (config options)
  master <- createMaster "10500"
  serverPids <- mapM (createServer (delay options)) config
  runProcess master $ do
    registerPeers serverPids
    liftIO $ threadDelay (sendFor options)
    mapM_ sendStop serverPids
    liftIO $ threadDelay (waitFor options)
    results <- mapM sendShutdown serverPids
    say $ show results
  -- Wait for logs to finish printing
  threadDelay 1000000
