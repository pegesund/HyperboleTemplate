{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module AppEffects.Logger (
  Logger (..),
  runLogger,
  withLogger,
  withLoggerLevel,
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

import Control.Exception (bracket)
import Data.Text (Text)
import qualified Data.Text as T
import Effectful
import Effectful.Dispatch.Dynamic
import System.IO (hPutStrLn, stderr)
import System.Log.Logger (Priority (..))
import qualified System.Log.Logger as Log

-- | The Logger effect
data Logger :: Effect where
  LogAt :: Priority -> String -> Logger m ()
  GetLoggerName :: Logger m String

type instance DispatchOf Logger = 'Dynamic

-- | Log a message at a specific level
logAt :: (Logger :> es, IOE :> es) => Priority -> String -> Eff es ()
logAt priority msg = send (LogAt priority msg)

-- | Get the current logger name
getLoggerName :: (Logger :> es) => Eff es String
getLoggerName = send GetLoggerName

-- | Log a message at DEBUG level
logDebug :: (Logger :> es, IOE :> es) => String -> Eff es ()
logDebug = logAt DEBUG

-- | Log a message at INFO level
logInfo :: (Logger :> es, IOE :> es) => String -> Eff es ()
logInfo = logAt INFO

-- | Log a message at WARNING level
logWarning :: (Logger :> es, IOE :> es) => String -> Eff es ()
logWarning = logAt WARNING

-- | Log a message at ERROR level
logError :: (Logger :> es, IOE :> es) => String -> Eff es ()
logError = logAt ERROR

-- | Log a message at CRITICAL level
logCritical :: (Logger :> es, IOE :> es) => String -> Eff es ()
logCritical = logAt CRITICAL

-- | Run the Logger effect with a specified logger name
runLogger :: (IOE :> es) => String -> Eff (Logger : es) a -> Eff es a
runLogger loggerName = interpret $ \_ -> \case
  LogAt priority msg -> liftIO $ Log.logM loggerName priority msg
  GetLoggerName -> pure loggerName

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
  Log.updateGlobalLogger appName (Log.setLevel logPriority)

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
