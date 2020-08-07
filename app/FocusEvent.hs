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


-- Convenience function for constructing a new event
mkEvent :: MonadIO m => Text -> Text -> m FocusEvent
mkEvent title inst = (FocusEvent <$> liftIO now <*> pure title <*> pure inst)

