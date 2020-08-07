{-# LANGUAGE TemplateHaskell #-}

module OrphanLenses where

import BasicPrelude
import I3IPC.Event (Event(Window), WindowEvent, WindowChange)
import I3IPC.Reply (Node)
import Lens.Micro (Traversal')
import Lens.Micro.TH (makeLensesWith)
import LensConfig (myRules)


makeLensesWith myRules ''Event
makeLensesWith myRules ''WindowEvent
makeLensesWith myRules ''WindowChange
makeLensesWith myRules ''Node

-- Not sure why this isn't being generated for us automatically, but oh well...
_Window :: Traversal' Event WindowEvent
_Window f (Window ev) = Window <$> f ev
_Window _ ev = pure ev

