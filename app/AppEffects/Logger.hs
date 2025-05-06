{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

module AppEffects.Logger (
  -- * Logger Effect
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

  -- * Performance logging
  withPerformanceLogging,
  showDuration,

  -- * HTTP Request/Response logging
  logHttpRequest,
  logHttpResponse,

  -- * Effect runners (previously in Runner.hs)
  runAppEffects,
  runAppEffectsWithPool,

  -- * Re-exported types
  Priority (..),
) where

import Control.Exception (bracket)
import qualified Data.ByteString.Char8 as BS
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Reader.Static (runReader)
import qualified Hasql.Pool as Pool
import Network.HTTP.Types (Status (statusCode))
import Network.Wai (Request, queryString, rawPathInfo, remoteHost, requestMethod)
import System.IO (hPutStrLn, stderr)
import System.Log.Logger (Priority (..))
import qualified System.Log.Logger as Log

-- Import other effects
import AppEffects.Config (AppConfig, Config, runConfig)
import AppEffects.DB (DB)

-- | The Logger effect
data Logger :: Effect where
  LogAt :: Priority -> String -> Logger m ()
  GetLoggerName :: Logger m String

type instance DispatchOf Logger = 'Dynamic

-- | Log a message at a specific level
logAt :: (Logger :> es, IOE :> es) => Priority -> String -> Eff es ()
logAt priority msg = send (LogAt priority msg)

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

-- | Execute an operation with performance logging
withPerformanceLogging ::
  (Logger :> es, IOE :> es) =>
  -- | Operation name to log
  String ->
  -- | The operation to measure
  Eff es a ->
  Eff es a
withPerformanceLogging operationName action = do
  logDebug $ "Starting operation: " ++ operationName
  startTime <- liftIO getCurrentTime

  result <- action

  endTime <- liftIO getCurrentTime
  let duration = realToFrac $ diffUTCTime endTime startTime * 1000 -- convert to ms

  -- Log performance based on duration thresholds
  if duration > 1000
    then logWarning $ "Slow operation: " ++ operationName ++ " took " ++ showDuration duration
    else logDebug $ "Completed operation: " ++ operationName ++ " in " ++ showDuration duration

  pure result

-- | Run the application effects stack (Config and Logger)
runAppEffects ::
  (IOE :> es) =>
  -- \^ IO Effect needed for Logger

  -- | The application configuration
  AppConfig ->
  -- | The logger name
  String ->
  -- | The effect to run
  Eff (Logger : Config : es) a ->
  Eff es a
runAppEffects config loggerName =
  runConfig config . runLogger loggerName

-- | Run the application effects stack with a database pool
runAppEffectsWithPool ::
  (IOE :> es) =>
  -- \^ IO Effect needed for Logger

  -- | The application configuration
  AppConfig ->
  -- | The logger name
  String ->
  -- | The database connection pool
  Pool.Pool ->
  -- | The effect to run
  Eff (DB : Logger : Config : es) a ->
  Eff es a
runAppEffectsWithPool config loggerName pool =
  runConfig config . runLogger loggerName . runReader pool

-- | Log an HTTP request
logHttpRequest :: (Logger :> es, IOE :> es) => Request -> Eff es ()
logHttpRequest req = do
  let method = BS.unpack $ requestMethod req
  let path = BS.unpack $ rawPathInfo req
  let query = queryStringToText $ queryString req
  let clientIp = show $ remoteHost req

  logInfo $ "HTTP Request: " ++ method ++ " " ++ path ++ query ++ " from " ++ clientIp

-- | Format duration in ms for logging
showDuration :: Double -> String
showDuration ms
  | ms < 1 = show (round (ms * 1000)) ++ "μs" -- microseconds
  | ms < 1000 = show (round ms) ++ "ms"
  | otherwise = show (ms / 1000) ++ "s"

-- | Log an HTTP response
logHttpResponse :: (Logger :> es, IOE :> es) => Status -> Double -> Eff es ()
logHttpResponse status durationMs = do
  let statusStr = show $ statusCode status
  logInfo $ "HTTP Response: " ++ statusStr ++ " (took " ++ showDuration durationMs ++ ")"

-- | Helper to format query string
queryStringToText :: [(BS.ByteString, Maybe BS.ByteString)] -> String
queryStringToText [] = ""
queryStringToText qs =
  let queryPairs = map formatParam qs
   in "?" ++ BS.unpack (BS.intercalate "&" queryPairs)
 where
  formatParam (k, Nothing) = k
  formatParam (k, Just v) = k <> "=" <> v
