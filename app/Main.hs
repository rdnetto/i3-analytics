module Main where

import BasicPrelude
import I3IPC (subscribe)
import qualified I3IPC.Subscribe as Sub

main :: IO ()
main = subscribe print [Sub.Window]
