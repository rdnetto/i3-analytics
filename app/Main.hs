module Main where

import BasicPrelude
import Chronos
import CLI
import Data.Semigroup (stimes)
import Options.Applicative (execParser)
import Torsor (add, invert)

import Analyse (analyseEvents)
import Subscribe (subscribeEvents)


main :: IO ()
main = do
  cmd <- execParser cliParser

  case cmd of
       CliSubscribe -> subscribeEvents
       CliAnalyse period -> analyseEvents =<< getTimeCond period

getTimeCond :: CliPeriod -> IO (Time -> Bool)
getTimeCond period = do
  startTime <- dayToTimeMidnight <$> getStartTime period
  return (> startTime)


getStartTime :: CliPeriod -> IO Day
getStartTime CliAll = pure $ Day 0
getStartTime CliDay = today
getStartTime CliWeek = f <$> now where
  f t0 = res where
    DayOfWeek today = timeToDayOfWeek t0
    weekLength = stimes today day

    res
      = timeToDayTruncate
      $ (invert weekLength) `add` t0

