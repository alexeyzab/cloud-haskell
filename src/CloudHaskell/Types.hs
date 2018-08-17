{-# LANGUAGE DeriveGeneric #-}

module CloudHaskell.Types where

import Control.Concurrent (MVar, newEmptyMVar)
import Data.Binary (Binary)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import System.Random.MWC (GenIO)

newtype Number = Number Double
  deriving (Show, Generic, Typeable)

instance Binary Number

data AppState = AppState
  { appStateReceivedNumbers :: [Double]
  , appStateTimeout :: MVar ()
  , appStateSeed :: GenIO
  }

data Timeout = Timeout
  deriving (Show, Generic, Typeable)

instance Binary Timeout

data Shutdown = Shutdown
  deriving (Show, Generic, Typeable)

instance Binary Shutdown

mkAppState :: GenIO -> IO AppState
mkAppState seed = do
  timeout <- newEmptyMVar
  pure AppState
    { appStateReceivedNumbers = []
    , appStateTimeout = timeout
    , appStateSeed = seed
    }
