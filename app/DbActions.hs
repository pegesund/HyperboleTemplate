{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module DbActions (
  -- * Pool initialization
  initPool,
  initPoolWithConfig,
  
  -- * Resource management
  withPool,
  withConfiguredPool,
  
  -- * Database sessions
  setupDbSession,
  storeMessageSession,
  getRecentMessagesSession,
) where

import AppEffects.Config (Config, getDbHost, getDbName, getDbUser, getDbPassword, getPoolSize)
import AppEffects.Logger (Logger, logDebug, logInfo)
import Control.Exception (bracket)
import Data.ByteString (ByteString)
import Data.Int (Int32)
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import Data.Time.Clock (secondsToDiffTime)
import qualified Data.Vector as Vector
import Effectful
import Hasql.Connection ()
import qualified Hasql.Connection.Setting as Connection.Setting
import qualified Hasql.Connection.Setting.Connection as Connection.Setting.Connection
import Hasql.Decoders ()
import Hasql.Encoders ()
import qualified Hasql.Pool as Pool
import qualified Hasql.Pool.Config as Config
import qualified Hasql.Session as Session
import qualified Hasql.Statement as HS
import Hasql.TH (resultlessStatement, vectorStatement)
import qualified System.Log.Logger as Log

-- Initialize the connection pool settings with explicit parameters
initPoolSettings :: ByteString -> Int -> Int -> Int -> Int -> Config.Config
initPoolSettings connStr size acqTimeout lifetime idletime =
  Config.settings
    [ Config.size size
    , Config.acquisitionTimeout (secondsToDiffTime $ fromIntegral acqTimeout)
    , Config.agingTimeout (secondsToDiffTime $ fromIntegral lifetime)
    , Config.idlenessTimeout (secondsToDiffTime $ fromIntegral idletime)
    , Config.staticConnectionSettings
        [ Connection.Setting.connection
            (Connection.Setting.Connection.string (TE.decodeUtf8 connStr))
        ]
    ]

-- Initialize the connection pool using config parameters
initPoolWithConfig :: ByteString -> Int -> Int -> Int -> Int -> IO Pool.Pool
initPoolWithConfig connStr size acqTimeout lifetime idletime = do
  Log.infoM "DbActions" $
    "Creating database connection pool (size: "
      ++ show size
      ++ ", acquisition timeout: "
      ++ show acqTimeout
      ++ "s"
      ++ ", max lifetime: "
      ++ show lifetime
      ++ "s"
      ++ ", max idle time: "
      ++ show idletime
      ++ "s)"

  pool <- Pool.acquire (initPoolSettings connStr size acqTimeout lifetime idletime)
  Log.infoM "DbActions" "Connection pool created successfully"

  -- Setup database table if needed
  setupDbResult <- Pool.use pool setupDbSession
  case setupDbResult of
    Left err -> do
      Log.errorM "DbActions" $ "Database setup error: " ++ show err
      Log.warningM "DbActions" "Continuing without database setup"
    Right _ -> Log.infoM "DbActions" "Database ready"

  pure pool

-- Initialize pool with legacy hard-coded values (for backward compatibility)
initPool :: IO Pool.Pool
initPool = do
  let connectionString = "host=localhost dbname=hyperbole user=hyperbole password=hyperbole"
  let poolSize = 10
  let acquisitionTimeout = 10 -- seconds
  let maxLifetime = 600 -- seconds (10 minutes)
  let maxIdletime = 600 -- seconds (10 minutes)
  initPoolWithConfig connectionString poolSize acquisitionTimeout maxLifetime maxIdletime

-- Run a function with a database connection pool, ensuring resources are cleaned up
withPool :: (Pool.Pool -> IO a) -> IO a
withPool =
  bracket
    initPool
    ( \pool -> do
        Log.infoM "DbActions" "Shutting down connection pool"
        Pool.release pool
        Log.infoM "DbActions" "Connection pool shutdown complete"
    )

-- Run a function with a database connection pool created from config
withConfiguredPool :: (Config :> es, Logger :> es, IOE :> es) => (Pool.Pool -> Eff es a) -> Eff es a
withConfiguredPool action = do
  logDebug "Setting up connection pool from config"
  
  -- Get configuration values from the Config effect
  dbHost <- getDbHost
  dbName <- getDbName
  dbUser <- getDbUser
  dbPassword <- getDbPassword
  poolSize <- getPoolSize
  
  -- Construct the connection string
  let connStr = TE.encodeUtf8 $ "host=" <> dbHost <> " dbname=" <> dbName <> " user=" <> dbUser <> " password=" <> dbPassword
  let acqTimeout = 10
  let lifetime = 600
  let idletime = 600
  
  pool <- liftIO $ initPoolWithConfig connStr poolSize acqTimeout lifetime idletime
  logInfo "Connection pool created, executing action"
  result <- action pool
  liftIO $ do
    Log.infoM "DbActions" "Shutting down connection pool"
    Pool.release pool
    Log.infoM "DbActions" "Connection pool shutdown complete"
  pure result

-- Setup database tables
setupDbSession :: Session.Session ()
setupDbSession =
  Session.sql $
    "CREATE TABLE IF NOT EXISTS messages (id SERIAL PRIMARY KEY, content TEXT NOT NULL, created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP)"

-- Save message to database
storeMessageSession :: Text -> Session.Session ()
storeMessageSession msg = Session.statement msg storeMessageStatement

-- SQL statement for storing messages using Template Haskell
storeMessageStatement :: HS.Statement Text ()
storeMessageStatement =
  [resultlessStatement|
    insert into messages (content) values ($1 :: text)
  |]

-- SQL statement for retrieving messages using Template Haskell
getMessagesStatement :: HS.Statement () (Vector.Vector (Int32, Text, Text))
getMessagesStatement =
  [vectorStatement|
    select id :: int4, content :: text, created_at :: text
    from messages
    order by created_at desc
    limit 5
  |]

-- Function to retrieve recent messages
getRecentMessagesSession :: Session.Session [(Int32, Text, Text)]
getRecentMessagesSession = do
  resultVector <- Session.statement () getMessagesStatement
  pure $ Vector.toList resultVector