module Main where

import CloudHaskell
import Data.Semigroup ((<>))
import Data.Vector (iterateN)
import Data.Word (Word32)
import System.Random.MWC (createSystemRandom, initialize, GenIO)
import Options.Applicative


data CommandLineArgs = CommandLineArgs
  { sendFor :: Int
  , waitFor :: Int
  , withSeed :: Maybe Word32
  } deriving Show

commandLineArgs :: Parser CommandLineArgs
commandLineArgs =
  CommandLineArgs
    <$> option auto
      ( long "send-for"
      <> metavar "INT"
      <> help "How many seconds to send messages for" )
    <*> option auto
      ( long "wait-for"
      <> metavar "INT"
      <> help "Length of the grace period in seconds" )
    <*> (optional $ option auto
      ( long "with-seed"
      <> metavar "SEED"
      <> help "Seed for RNGs" ))

main :: IO ()
main = do
  CommandLineArgs{..} <- execParser opts
  seed <- maybe createSystemRandom (initialize . iterateN 255 succ) withSeed
  start sendFor waitFor seed
  where
    opts = info (commandLineArgs <**> helper)
      ( fullDesc
      <> progDesc "Run the cloud-haskell app"
      <> header "An example of nodes sending messages to each other" )
