{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Logger (
  -- * Logger Effect
  LogEnv,
  initLogger,
  withLogger,
  withLoggerLevel,
  initLoggerFromConfig,
  textToPriority,

  -- * Logging functions
  logDebug,
  logInfo,
  logWarning,
  logError,
  logCritical,

  -- * Re-exported types
  Priority (..),
) where

import System.IO (hPutStrLn, stderr)
import System.Log.Logger (Priority (..), setLevel, updateGlobalLogger)
import qualified System.Log.Logger as Log

import Control.Exception (bracket)
import Data.Text (Text)
import qualified Data.Text as T

import Effectful
import Effectful.Reader.Static

import Config (ConfigEnv, getLogLevel)

-- | Effect for providing access to the logger
type LogEnv = Reader String

-- | Convert text log level to Priority
textToPriority :: Text -> Priority
textToPriority level = case T.toUpper level of
  "DEBUG" -> DEBUG
  "INFO" -> INFO
  "NOTICE" -> NOTICE
  "WARNING" -> WARNING
  "ERROR" -> ERROR
  "CRITICAL" -> CRITICAL
  "ALERT" -> ALERT
  "EMERGENCY" -> EMERGENCY
  _ -> INFO -- Default to INFO if level is unknown

-- | Initialize the logger and return the logger object
initLogger :: String -> Priority -> IO String
initLogger appName logPriority = do
  -- Set up the logger with the given app name and log level
  updateGlobalLogger appName (setLevel logPriority)

  -- Return the app logger name for use in the effect system
  return appName

-- | Run an action with a configured logger and default log level (INFO)
withLogger :: String -> (String -> IO a) -> IO a
withLogger appName =
  bracket
    (initLogger appName INFO)
    ( \_ -> do
        hPutStrLn stderr "Shutting down logger"
    )

-- | Run an action with a configured logger and specified log level
withLoggerLevel :: String -> Priority -> (String -> IO a) -> IO a
withLoggerLevel appName logPriority =
  bracket
    (initLogger appName logPriority)
    ( \_ -> do
        hPutStrLn stderr "Shutting down logger"
    )

-- | Initialize logger with level from config (for use within effectful code)
initLoggerFromConfig :: (ConfigEnv :> es, IOE :> es) => String -> Eff es String
initLoggerFromConfig appName = do
  -- Get log level from config
  logLevelText <- getLogLevel
  let logPriority = textToPriority logLevelText

  -- Initialize logger with the log level from config
  liftIO $ initLogger appName logPriority

-- | Log a message at DEBUG level
logDebug :: (LogEnv :> es, IOE :> es) => String -> Eff es ()
logDebug msg = do
  loggerName <- ask
  liftIO $ Log.debugM loggerName msg

-- | Log a message at INFO level
logInfo :: (LogEnv :> es, IOE :> es) => String -> Eff es ()
logInfo msg = do
  loggerName <- ask
  liftIO $ Log.infoM loggerName msg

-- | Log a message at WARNING level
logWarning :: (LogEnv :> es, IOE :> es) => String -> Eff es ()
logWarning msg = do
  loggerName <- ask
  liftIO $ Log.warningM loggerName msg

-- | Log a message at ERROR level
logError :: (LogEnv :> es, IOE :> es) => String -> Eff es ()
logError msg = do
  loggerName <- ask
  liftIO $ Log.errorM loggerName msg

-- | Log a message at CRITICAL level
logCritical :: (LogEnv :> es, IOE :> es) => String -> Eff es ()
logCritical msg = do
  loggerName <- ask
  liftIO $ Log.criticalM loggerName msg
