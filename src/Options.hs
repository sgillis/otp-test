module Options (Options(..), options) where

import Data.Semigroup ((<>))
import Options.Applicative

data Options = Options
  { sendFor :: Int
  , waitFor :: Int
  , delay :: Int
  , config :: String
  }

options :: ParserInfo Options
options =
  info (parser <**> helper)
  ( fullDesc
    <> progDesc "CH/OTP test task"
  )

parser :: Parser Options
parser = Options
  <$> option auto
      ( long "send-for"
        <> help "How long the system sends messages in ns"
      )
  <*> option auto
      ( long "wait-for"
        <> help "Grace period in ns"
      )
  <*> option auto
      ( long "send-delay"
        <> value 50000
        <> help "Timeout in ns before a server generates a new message"
      )
  <*> strOption
      ( long "config"
        <> value "config"
        <> help "Name of the cluster config file"
      )
