module Main where

import BasicPrelude
import Chronos (Time, now)
import CLI
import Data.Aeson (encode)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Map.Strict as DMS
import FocusEvent
import I3IPC (subscribe)
import I3IPC.Event (Event)
import I3IPC.Reply (WindowProperty(Title, Instance), Node(node_window_properties))
import qualified I3IPC.Subscribe as Sub
import Lens.Micro ((^?), (^.), _Just, at)
import Lens.Micro.GHC ()
import Options.Applicative (execParser)
import OrphanLenses
import Safe (fromJustNote)
import System.Directory (XdgDirectory(XdgData), getXdgDirectory, createDirectoryIfMissing)
import System.IO (Handle, IOMode(AppendMode), hPutStrLn, withFile)


main :: IO ()
main = do
  cmd <- execParser cliParser
  case cmd of
       CliSubscribe -> subscribeEvents
       CliAnalyse -> error "Not implemented"

subscribeEvents :: IO ()
subscribeEvents = do
  configDir <- getXdgDirectory XdgData "i3-analytics"
  createDirectoryIfMissing True configDir
  let configFile = configDir </> "focus_events.jsonl"

  withFile configFile AppendMode $ \h ->
    subscribe (handleEvent $ appendEntry h) [Sub.Window]


handleEvent :: (FocusEvent -> IO ()) -> Either String Event -> IO ()
handleEvent _ (Left err) = error err
handleEvent output (Right event)
    = output =<< (FocusEvent <$> now <*> pure title <*> pure instance')
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


appendEntry :: Handle -> FocusEvent -> IO ()
appendEntry h record = do
  BSL.hPut h (encode record)
  hPutStrLn h ""

