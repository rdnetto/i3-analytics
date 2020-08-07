{-# LANGUAGE DeriveGeneric #-}

module FocusEvent where

import BasicPrelude
import Chronos (Time, now)
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)


data FocusEvent = FocusEvent {
  feTimestamp :: Time,
  feTitle :: Text,
  feInstance :: Text
} deriving (Eq, Show, Generic)

instance FromJSON FocusEvent
instance ToJSON FocusEvent

