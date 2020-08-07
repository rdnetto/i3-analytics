module Subscribe.LockEvents where

import BasicPrelude
import Control.Concurrent (threadDelay)
import Control.Monad.State.Strict (get, put, runStateT)
import Data.Function ((&))
import System.Directory (doesPathExist, getSymbolicLinkTarget, listDirectory)

import FocusEvent



-- Known screen locker processes to match
screenLockers :: [FilePath]
screenLockers = [
    "/usr/bin/i3lock",
    "/usr/bin/swaylock"
  ]


-- Subscribes to screen lock/unlock events.
-- This works by polling, because /proc doesn't support inotify.
-- the state represents "is the screen currently locked?"
subscribeScreenLock :: (FocusEvent -> IO ()) -> IO ()
subscribeScreenLock handler
  = void
  . flip runStateT False
  . forever
  $ do
    wasLocked <- get
    isLocked <- isScreenLocked

    when (isLocked && isLocked /= wasLocked)
      $ liftIO . handler =<< mkEvent "_screen-locked" "_screen-locked"

    put isLocked

    liftIO $ threadDelay 1000000 -- 1 sec


-- Check if a screenlocker process is running
isScreenLocked :: MonadIO m => m Bool
isScreenLocked = lockerPresent <$> liftIO getExes where
  exePath pid = "/proc/" ++ pid ++ "/exe"

  getExes = listDirectory "/proc"
    &   map (map exePath)
    >>= filterM doesPathExist
    >>= mapM getSymbolicLinkTarget

  -- Using list monad to check for the presence of screen locker process
  lockerPresent exes = or $ (==) <$> screenLockers <*> exes

