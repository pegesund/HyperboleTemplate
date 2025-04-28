{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module DbPool 
  ( initPool
  , withPool
  , DB
  , setupDbSession
  , storeMessageSession
  , getRecentMessagesSession
  ) where

import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import Data.ByteString (ByteString)
import Data.Int (Int32)
import qualified Data.Vector as Vector
import Data.Time.Clock (secondsToDiffTime)
import Effectful()
import Effectful.Reader.Static
import qualified Hasql.Pool as Pool
import qualified Hasql.Pool.Config as Config
import Hasql.Connection()
import qualified Hasql.Connection.Setting as Connection.Setting
import qualified Hasql.Connection.Setting.Connection as Connection.Setting.Connection
import qualified Hasql.Session as Session
import qualified Hasql.Statement as HS
import Hasql.Decoders()
import Hasql.Encoders()
import Hasql.TH (resultlessStatement, vectorStatement)
import Logger (logInfo)
import Control.Exception (bracket)

-- Effect for database access using a connection pool
type DB = Reader Pool.Pool

-- Database connection settings
connectionString :: ByteString
connectionString = "host=localhost dbname=hyperbole user=hyperbole password=hyperbole"

-- Pool configuration
poolSize :: Int
poolSize = 10

acquisitionTimeout :: Int
acquisitionTimeout = 10  -- seconds

maxLifetime :: Int
maxLifetime = 600  -- seconds (10 minutes)

maxIdletime :: Int
maxIdletime = 600  -- seconds (10 minutes)

-- Initialize the connection pool settings
initPoolSettings :: Config.Config
initPoolSettings = Config.settings
  [ Config.size poolSize
  , Config.acquisitionTimeout (secondsToDiffTime $ fromIntegral acquisitionTimeout)
  , Config.agingTimeout (secondsToDiffTime $ fromIntegral maxLifetime)
  , Config.idlenessTimeout (secondsToDiffTime $ fromIntegral maxIdletime)
  , Config.staticConnectionSettings
      [ Connection.Setting.connection
          (Connection.Setting.Connection.string (TE.decodeUtf8 connectionString))
      ]
  ]

-- Initialize the connection pool and return it
initPool :: IO Pool.Pool
initPool = do
  logInfo $ "Creating database connection pool (size: " ++ show poolSize ++ 
           ", acquisition timeout: " ++ show acquisitionTimeout ++ "s" ++
           ", max lifetime: " ++ show maxLifetime ++ "s" ++
           ", max idle time: " ++ show maxIdletime ++ "s)"
           
  pool <- Pool.acquire initPoolSettings
  logInfo "Connection pool created successfully"
  
  -- Setup database table if needed
  setupDbResult <- Pool.use pool setupDbSession
  case setupDbResult of
    Left err -> do
      logInfo $ "Database setup error: " ++ show err
      logInfo "Continuing without database setup"
    Right _ -> logInfo "Database ready"
  
  pure pool

-- Run a function with a database connection pool, ensuring resources are cleaned up
withPool :: (Pool.Pool -> IO a) -> IO a
withPool = bracket 
  initPool
  (\pool -> do
    logInfo "Shutting down connection pool"
    Pool.release pool
    logInfo "Connection pool shutdown complete")

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