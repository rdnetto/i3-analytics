module Main where

import BasicPrelude
import CLI
import Options.Applicative (execParser)

import Analyse (analyseEvents)
import Subscribe (subscribeEvents)


main :: IO ()
main = do
  cmd <- execParser cliParser

  case cmd of
       CliSubscribe -> subscribeEvents
       CliAnalyse -> analyseEvents

