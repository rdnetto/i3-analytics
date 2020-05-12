module CLI where

import BasicPrelude
import Options.Applicative


data CLI
  = CliSubscribe
  | CliAnalyse
  deriving (Eq, Show)


cliParser :: ParserInfo CLI
cliParser
  = info (rootP <* helper) mempty
  where
    rootP = hsubparser $ subscribe <> analyse
    subscribe
      = command "subscribe"
      . info (pure CliSubscribe)
      $ progDesc "Subscribe to i3 events, logging them"
    analyse
      = command "analyse"
      . info (pure CliAnalyse)
      $ progDesc "Analyse logged i3 events"

