module CLI where

import BasicPrelude
import Options.Applicative


data CLI
  = CliSubscribe
  | CliAnalyse CliPeriod
  deriving (Eq, Show)

-- The period of data to analyse
data CliPeriod
  = CliDay
  | CliWeek
  | CliAll
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
      . info analyseSubcmd
      $ progDesc "Analyse logged i3 events"

    analyseSubcmd = hsubparser $ dayCmd <> weekCmd <> allCmd
    dayCmd
      = command "today"
      . info (pure $ CliAnalyse CliDay)
      $ progDesc "Analyse today's usage"
    weekCmd
      = command "this-week"
      . info (pure $ CliAnalyse CliWeek)
      $ progDesc "Analyse this week's usage"
    allCmd
      = command "all"
      . info (pure $ CliAnalyse CliAll)
      $ progDesc "Analyse all recorded usage"

