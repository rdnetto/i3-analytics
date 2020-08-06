module Subscribe where

import BasicPrelude
import Chronos (Time, now)
import Control.Concurrent (myThreadId)
import Control.Exception (finally, throwTo)
import Data.Aeson (encode)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Map.Strict as DMS
import I3IPC (subscribe)
import I3IPC.Event (Event)
import I3IPC.Reply (WindowProperty(Title, Instance), Node(node_window_properties))
import qualified I3IPC.Subscribe as Sub
import Lens.Micro ((^?), (^.), _Just, at)
import Lens.Micro.GHC ()
import Safe (fromJustNote)
import System.Exit(ExitCode(ExitSuccess))
import System.IO (Handle, IOMode(AppendMode), hPutStrLn, withFile)
import System.Posix.Signals (Handler(..), installHandler, sigTERM)

import ConfigFile
import FocusEvent
import OrphanLenses


subscribeEvents :: IO ()
subscribeEvents = do
  configFile' <- configFile

  withFile configFile' AppendMode $ \h -> do
    -- Gracefully handle SIGTERM
    -- Note that SIGINT already works via throwing an exception, so no additional logic is needed
    mainThread <- myThreadId
    installHandler sigTERM (Catch $ throwTo mainThread ExitSuccess) Nothing

    finally
        (subscribe (handleEvent $ appendEntry h) [Sub.Window])
        (onExit h)


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

onExit :: Handle -> IO ()
onExit h = do
    t0 <- now
    appendEntry h $ FocusEvent t0 "_terminated" "_terminated"

