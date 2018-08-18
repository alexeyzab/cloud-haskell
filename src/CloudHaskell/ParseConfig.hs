{-# LANGUAGE OverloadedStrings #-}

module CloudHaskell.ParseConfig where

import Data.Yaml
import qualified Data.ByteString.Char8 as BS
import Control.Exception.Safe (throwIO)

data Config = Config
  { configMainHost :: String
  , configMainPort :: String
  , configNodes :: [String]
  } deriving Show

instance FromJSON Config where
  parseJSON (Object v) =
    Config <$>
    v .: "mainHost" <*>
    v .: "mainPort" <*>
    v .: "nodes"
  parseJSON _ = fail "Expected Object for Config value"

parseConfig :: IO Config
parseConfig = do
  file <- BS.readFile "config.yaml"
  case decodeEither' file of
    Left e -> throwIO . userError $ "Coudln't parse the config file: " ++ show e
    Right c -> pure c
