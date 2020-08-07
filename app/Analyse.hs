module Analyse where

import BasicPrelude hiding (second)
import Chronos
import Control.Monad.State as S
import qualified Data.HashMap.Strict as HMS
import Safe (headDef)
import Torsor (difference, invert)

import ConfigFile
import FocusEvent
import JSONL (decodeFile)
import LabelRules

analyseEvents :: (Time -> Bool) -> IO ()
analyseEvents cond = do
  rows <- filter ((/= "_terminated") . feInstance) . filter (cond . feTimestamp) <$> loadRows

  -- Label & combine rows
  let labels
        = sortBy (compare `on` (invert . snd))
        . filter ((> second) . snd)
        . HMS.toList
        . HMS.fromListWith (<>)
        $ zipWith labelDuration rows (tail rows)

  forM_ labels $ \(label, duration) ->
      putStrLn $ label ++ ": " ++ tsPrettyPrint duration


loadRows :: IO [FocusEvent]
loadRows = do
  res :: Either String [FocusEvent]
    <- decodeFile =<< configFile

  return $ case res of
                Left err -> error err
                Right x  -> x


-- Given a pair of events, emit a labelled description for the time between them
labelDuration :: FocusEvent -> FocusEvent -> (Text, Timespan)
labelDuration startEvent endEvent = (label, duration) where
    duration = (feTimestamp endEvent) `difference` (feTimestamp startEvent)
    label
      = headDef defLabel
      . catMaybes
      $ map ($ startEvent) labelRules
    -- Fallback: just use the application ID
    defLabel = feInstance startEvent

tsPrettyPrint :: Timespan -> Text
tsPrettyPrint ts = mconcat res where
    units = [(week, "w"), (day, "d"), (hour, "h"), (minute, "m"), (second, "s")]
    res = (flip S.evalState) (getTimespan ts)
        . forM units
        $ \(amt, desc) -> do
            let amt' = getTimespan amt
            current <- S.get
            let qty = current `div` amt'
            S.modify (\x -> x - (amt' * qty))

            let x | qty > 0   = tshow qty ++ desc ++ " "
                  | otherwise = ""
            return x

