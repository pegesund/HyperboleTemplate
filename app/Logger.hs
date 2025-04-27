{-# LANGUAGE OverloadedStrings #-}
module Logger where

import System.IO (hPutStrLn, stderr)
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (formatTime, defaultTimeLocale)
import Control.Monad.IO.Class (MonadIO, liftIO)

-- Simple logger function
logInfo :: String -> IO ()
logInfo msg = do
  time <- getCurrentTime
  let timestamp = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" time
  hPutStrLn stderr $ "[INFO] " ++ timestamp ++ " - " ++ msg

-- Wrapper for liftIO $ logInfo
logInfoM :: MonadIO m => String -> m ()
logInfoM = liftIO . logInfo