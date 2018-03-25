{-# LANGUAGE DeriveGeneric #-}
module Server where

import Control.Concurrent (threadDelay)
import Control.Distributed.Process hiding (call)
import Control.Distributed.Process.Extras (ExitReason(..))
import Control.Distributed.Process.Extras.Time (Delay(..))
import Control.Distributed.Process.ManagedProcess
import Control.Distributed.Process.ManagedProcess.Server
import Data.Binary
import Data.Typeable (Typeable)
import GHC.Generics
import System.Clock
import System.Random

import Message

data SetPeers = SetPeers [ProcessId]
  deriving (Typeable, Generic, Eq, Show)

instance Binary SetPeers where
  put (SetPeers pid) = put pid
  get = SetPeers <$> get

data SendMessage = SendMessage
  deriving (Typeable, Generic, Eq, Show)

instance Binary SendMessage where

data ReceiveMessage = ReceiveMessage M
  deriving (Typeable, Generic, Eq, Show)

instance Binary ReceiveMessage where
  put (ReceiveMessage x) = put x
  get = ReceiveMessage <$> get

data RelayMessage = RelayMessage M
  deriving (Typeable, Generic, Eq, Show)

instance Binary RelayMessage where
  put (RelayMessage x) = put x
  get = RelayMessage <$> get

data GetMessages = GetMessages
  deriving (Typeable, Generic, Eq, Show)

instance Binary GetMessages where

data SendStop = SendStop
  deriving (Typeable, Generic, Eq, Show)

instance Binary SendStop where

data Shutdown = Shutdown
  deriving (Typeable, Generic, Eq, Show)

instance Binary Shutdown where

data State =
  State { peers :: [ProcessId]
        , messages :: [M]
        , rng :: StdGen
        , self :: ProcessId
        , active :: Bool
        , sendDelay :: Int
        } deriving Show

setPeers :: ProcessId -> [ProcessId] -> Process [ProcessId]
setPeers sid pids = call sid (SetPeers pids)

sendMessage :: ProcessId -> Process ()
sendMessage sid = cast sid SendMessage

receiveMessage :: M -> ProcessId -> Process ()
receiveMessage m sid = cast sid (ReceiveMessage m)

relayMessage :: M -> ProcessId -> Process ()
relayMessage m sid = cast sid (RelayMessage m)

getMessages :: ProcessId -> Process [M]
getMessages sid = call sid GetMessages

sendStop :: ProcessId -> Process ()
sendStop sid = call sid SendStop

sendShutdown :: ProcessId -> Process (Int, Double)
sendShutdown pid = call pid Shutdown

serverDefinition :: ProcessDefinition State
serverDefinition = defaultProcess
  { apiHandlers =
      [ handleCall handleSetPeers
      , handleCall handleSendStop
      , handleCall handleSendShutdown
      , handleCall handleGetMessages
      , handleCast handleSendMessage
      , handleCast handleReceiveMessage
      , handleCast handleRelayMessage
      ]
  }

initHandler :: InitHandler (Int, Int) State
initHandler (seed, sendDelay) =
  let
    initState self = State { peers = []
                           , messages = []
                           , rng = mkStdGen seed
                           , self = self
                           , active = True
                           , sendDelay = sendDelay
                           }
  in do
    self <- getSelfPid
    return $ InitOk (initState self) Infinity

handleSetPeers :: CallHandler State SetPeers [ProcessId]
handleSetPeers state (SetPeers peers) =
  do
    newState <- return $ state { peers = peers }
    action <- continue newState
    sendMessage (self state)
    replyWith peers action

handleSendStop :: CallHandler State SendStop ()
handleSendStop state SendStop =
  continue (state { active = False }) >>= replyWith ()

handleSendShutdown :: CallHandler State Shutdown (Int, Double)
handleSendShutdown state Shutdown = do
  stop ExitShutdown >>= replyWith (calcResult $ sort $ messages state)

handleGetMessages :: CallHandler State GetMessages [M]
handleGetMessages state GetMessages =
  continue state >>= replyWith (messages state)

handleSendMessage :: CastHandler State SendMessage
handleSendMessage state SendMessage =
  let
    activeAction =
      if active state then
        do
          spawnLocal (
            liftIO (threadDelay (sendDelay state)) >>
            sendMessage (self state)
            )
          return ()
      else
        return ()
  in
    do
      t <- liftIO $ getTime Realtime
      (x, rng') <- return $ (random (rng state) :: (Double, StdGen))
      m <- return $ M x t
      newState <- return $ state { messages = merge (messages state) m
                                 , rng = rng'
                                 }
      mapM_ (receiveMessage m) (peers state)
      activeAction
      continue newState

handleReceiveMessage :: CastHandler State ReceiveMessage
handleReceiveMessage state (ReceiveMessage m) =
  let
    newState = state { messages = merge (messages state) m }
  in do
    mapM_ (relayMessage m) (peers state)
    continue newState

handleRelayMessage :: CastHandler State RelayMessage
handleRelayMessage state (RelayMessage m) =
  let
    newState = state { messages = merge (messages state) m }
  in do
    continue newState
