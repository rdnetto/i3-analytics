module Subscribe.I3 (subscribeI3) where

import BasicPrelude
import qualified Data.Map.Strict as DMS
import I3IPC (subscribe)
import I3IPC.Event (Event)
import I3IPC.Reply (WindowProperty(Title, Instance), Node(node_window_properties))
import qualified I3IPC.Subscribe as Sub
import Lens.Micro ((^?))
import Lens.Micro.GHC ()
import Safe (fromJustNote)

import FocusEvent
import OrphanLenses


subscribeI3 :: (FocusEvent -> IO ()) -> IO ()
subscribeI3 handler = subscribe (handleEvent handler) [Sub.Window]


handleEvent :: (FocusEvent -> IO ()) -> Either String Event -> IO ()
handleEvent _ (Left err) = error err
handleEvent output (Right event)
    = output =<< mkEvent title instance'
  where
    node :: Node
      =  fromJustNote ("Failed to process event: " ++ show event)
      $  event
      ^? _Window
      . _win_container

    winProps
      = fromJustNote ("Failed to extract properties for: " ++ show node)
      $  node_window_properties node

    -- title is the string we see displayed, instance is a string identifying the program
    lookupProp :: WindowProperty -> Text
    lookupProp prop
      =  fromJustNote ("Failed to extract properties for: " ++ show node)
      .  join
      $  DMS.lookup prop winProps

    title = lookupProp Title
    instance' = lookupProp Instance

