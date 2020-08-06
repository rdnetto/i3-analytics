module LabelRules(labelRules) where

import BasicPrelude hiding (isInfixOf)
import Data.Text (isInfixOf)

import FocusEvent


-- Rules for labelling applications - Just represents a match
labelRules :: [FocusEvent -> Maybe Text]
labelRules = [
      mkRule (\FocusEvent{..} -> feInstance == "chromium-browser-chromium" && "Pull Request" `isInfixOf` feTitle) "pull-request",
      mkRule (\FocusEvent{..} -> feInstance == "Navigator") "firefox"
    ]

mkRule :: (FocusEvent -> Bool) -> Text -> FocusEvent -> Maybe Text
mkRule cond label fe
  | cond fe   = Just label
  | otherwise = Nothing

