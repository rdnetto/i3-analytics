module Subscribe where

import BasicPrelude
import Control.Concurrent (forkIO, killThread, myThreadId)
import Control.Concurrent.Chan (newChan, readChan, writeChan)
import Control.Exception (finally, onException, throwTo)
import Data.Aeson (encode)
import qualified Data.ByteString.Lazy as BSL
import System.Exit(ExitCode(ExitSuccess))
import System.IO (Handle, IOMode(AppendMode), hPutStrLn, withFile)
import System.Posix.Signals (Handler(..), installHandler, sigTERM)

import ConfigFile
import FocusEvent
import Subscribe.I3
import Subscribe.LockEvents


subscribeEvents :: IO ()
subscribeEvents = do
  -- Gracefully handle SIGTERM
  -- Note that SIGINT already works via throwing an exception, so no additional logic is needed
  mainThread <- myThreadId
  void $ installHandler sigTERM (Catch $ throwTo mainThread ExitSuccess) Nothing

  configFile' <- configFile
  withFile configFile' AppendMode $ \h -> do
    logQueue <- newChan

    -- GHC runtime semantics: the process terminates when the main thread does, regardless of the state of child processes (which get no heads up)
    -- This means that we need our main thread to be processing the output, and spawn child threads for consuming event sources
    -- However, if the child threads die, we need to make sure we take the parent out too so we don't keep running in degraded state
    let forkChild f = void . forkIO $ onException f (killThread mainThread)
    forkChild . subscribeI3 $ writeChan logQueue
    forkChild . subscribeScreenLock $ writeChan logQueue

    finally
      (forever $ appendEntry h =<< readChan logQueue)
      (onExit h)


appendEntry :: Handle -> FocusEvent -> IO ()
appendEntry h record = do
  BSL.hPut h (encode record)
  hPutStrLn h ""

onExit :: Handle -> IO ()
onExit h = appendEntry h =<< mkEvent "_terminated" "_terminated"

