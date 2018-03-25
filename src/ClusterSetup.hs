module ClusterSetup (createMaster, createServer, registerPeers) where

import Control.Distributed.Process
import Control.Distributed.Process.Backend.SimpleLocalnet
import Control.Distributed.Process.ManagedProcess
import Control.Distributed.Process.Node hiding (newLocalNode)
import Network.Socket (ServiceName)

import Server

createMaster :: ServiceName -> IO LocalNode
createMaster sn =
  createBackend sn >>= newLocalNode

createServer :: Int -> (String, Int) -> IO ProcessId
createServer sendDelay (sn, seed) = do
  b <- createBackend sn
  node <- newLocalNode b
  startServer (seed, sendDelay) node

createBackend :: ServiceName -> IO Backend
createBackend sn =
  initializeBackend "127.0.0.1" sn initRemoteTable

startServer :: (Int, Int) -> LocalNode -> IO ProcessId
startServer init node =
  forkProcess node $ serve init initHandler serverDefinition

registerPeers :: [ProcessId] -> Process ()
registerPeers servers =
  mapM_ (\target -> register target (peers target)) servers
  where
    register :: ProcessId -> [ProcessId] -> Process [ProcessId]
    register target pids =
      setPeers target pids

    peers :: ProcessId -> [ProcessId]
    peers target =
      [pid | pid <- servers, pid /= target]
