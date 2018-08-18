module CloudHaskell where

import qualified Control.Distributed.Backend.P2P as P2P
import Control.Concurrent (threadDelay, takeMVar, putMVar)
import Control.Distributed.Process
import Control.Distributed.Process.Closure
import Control.Distributed.Process.Node (initRemoteTable)
import Control.Monad (forever, mapM_, liftM)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State
import System.Random.MWC (GenIO, uniformR)

import CloudHaskell.ParseConfig
import CloudHaskell.Types
import CloudHaskell.Utility

start :: Int -> Int -> GenIO -> Config -> IO ()
start sendFor waitFor seed (Config host port nodes) =
  P2P.bootstrap
    host
    port
    (const (host, port))
    initRemoteTable
    (map P2P.makeNodeId nodes) $ do
      -- A delay to discover the peers
      liftIO $ threadDelay (toMicroseconds 1)
      setupCommunication sendFor waitFor seed

processLabel :: String
processLabel = "numbers"

-- Generate a number and send it to all peers
sendNumbers :: StateT AppState Process ()
sendNumbers = forever $ do
  (AppState _ _ seed) <- get
  peers <- lift P2P.getPeers
  lift . say $ "Seeing the following peers: " ++ show peers
  nodeId <- lift getSelfNode
  lift . say $ show nodeId
  mTimeout <- lift $ receiveTimeout 0
    [ match $ \Timeout -> pure () ]

  case mTimeout of
    Just _ -> lift $ die ("Reached timeout!" :: String)
    Nothing -> pure ()

  number <- liftIO $ uniformR (0, 1) seed
  lift . say $ "Sending " ++ show number
  lift $ P2P.nsendPeers processLabel (Number number)
  liftIO $ threadDelay (toMilliseconds 4)

-- Receive numbers and add them to AppState
receiveNumbers :: StateT AppState Process ()
receiveNumbers = do
  (AppState m timeout _) <- get
  lift . say $ "All messages so far: " ++ show m
  md <- lift $ receiveWait
    [ match $ \(Number d) -> pure (Just d)
    , match $ \Shutdown -> pure Nothing
    ]
  case md of
    Nothing -> do
      lift . say $ "Received shutdown"
      lift . say $ "Answer: " ++ show (calculateAnswer m)
      liftIO $ putMVar timeout ()
    Just d -> do
      lift . say $ "Received: " ++ show d
      modify (\(AppState m' t s) -> AppState (d : m') t s)
      receiveNumbers

-- Setup sending and receiving
setupCommunication :: Int -> Int -> GenIO -> Process ()
setupCommunication sendFor waitFor seed = do
  state@(AppState _ timeout _) <- liftIO $ mkAppState seed
  receivingPid <- spawnLocal $ do
    self <- getSelfPid
    register processLabel self
    say $ "Started receiving on pid: " ++ show self
    evalStateT receiveNumbers state
  sendingPid <- spawnLocal $ evalStateT sendNumbers state
  _ <- spawnLocal $ checkTimeout sendFor waitFor sendingPid receivingPid
  liftIO $ takeMVar timeout
  liftIO $ threadDelay (toMicroseconds 2)

-- Use commandline args to determine when to stop sending numbers
checkTimeout :: Int -> Int -> ProcessId -> ProcessId -> Process ()
checkTimeout sendFor waitFor spid rpid = do
  say $ "Seconds remaining: " ++ show sendFor
  liftIO $ threadDelay (toMicroseconds sendFor)
  say "Stopping"
  send spid Timeout
  liftIO $ threadDelay (toMicroseconds waitFor)
  say "Shutting down"
  send rpid Shutdown
