module ConfigFile where

import BasicPrelude
import System.Directory (XdgDirectory(XdgData), getXdgDirectory, createDirectoryIfMissing)


configFile :: IO FilePath
configFile = do
  configDir <- getXdgDirectory XdgData "i3-analytics"
  createDirectoryIfMissing True configDir
  return $ configDir </> "focus_events.jsonl"

