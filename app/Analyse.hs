module Analyse where

import BasicPrelude

import ConfigFile
import FocusEvent
import JSONL (decodeFile)


analyseEvents :: IO ()
analyseEvents = do
  res :: Either String [FocusEvent]
    <- decodeFile =<< configFile

  let rows = case res of
                  Left err -> error err
                  Right x  -> x
  print rows

