{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Main where

import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import Data.ByteString (ByteString)
import Effectful
import Effectful.Reader.Static
import qualified Hasql.Pool as Pool
import qualified Hasql.Pool.Config as Config
import Hasql.Connection()
import qualified Hasql.Connection.Setting as Connection.Setting
import qualified Hasql.Connection.Setting.Connection as Connection.Setting.Connection
import qualified Hasql.Decoders as HD
import qualified Hasql.Encoders as HE
import qualified Hasql.Session as Session
import qualified Hasql.Statement as HS
import Logger (logInfo)
import Web.Hyperbole
import Control.Exception (bracket)

-- Effect for database access using a connection pool
type DB = Reader Pool.Pool

main :: IO ()
main = do
  -- Log application startup
  logInfo "Starting Hyperbole application on port 3000"

  -- Database connection settings
  let connectionString = "host=localhost dbname=hyperbole user=hyperbole password=hyperbole" :: ByteString
      poolSize = 10
      acquisitionTimeout = 10  -- seconds
      maxLifetime = 600       -- seconds
      maxIdletime = 600       -- seconds
      poolSettings = Config.settings
        [ Config.size poolSize
        , Config.acquisitionTimeout acquisitionTimeout
        , Config.agingTimeout maxLifetime
        , Config.idlenessTimeout maxIdletime
        , Config.staticConnectionSettings
            [ Connection.Setting.connection
                (Connection.Setting.Connection.string (TE.decodeUtf8 connectionString))
            ]
        ]
  
  -- Create and use a connection pool with proper resource management
  bracket
    -- Acquire the pool
    (do
      logInfo $ "Creating database connection pool (size: " ++ show poolSize ++ 
               ", acquisition timeout: " ++ show acquisitionTimeout ++ "s" ++
               ", max lifetime: " ++ show maxLifetime ++ "s" ++
               ", max idle time: " ++ show maxIdletime ++ "s)"
      pool <- Pool.acquire poolSettings
      logInfo "Connection pool created successfully"
      
      -- Setup database table if needed
      setupDbResult <- Pool.use pool setupDbSession
      case setupDbResult of
        Left err -> do
          logInfo $ "Database setup error: " ++ show err
          logInfo "Continuing without database setup"
        Right _ -> logInfo "Database ready"
      
      pure pool)
    
    -- Release the pool when done
    (\pool -> do
      logInfo "Shutting down connection pool"
      Pool.release pool
      logInfo "Connection pool shutdown complete")
    
    -- Use the pool
    (\pool -> do
      -- Run application with database connection pool
      logInfo "Starting application server on port 3000"
      run 3000 $ do
        liveApp
          (basicDocument "Example with Connection Pool")
          (runReader pool $ runPage mypage))

-- Setup database tables
setupDbSession :: Session.Session ()
setupDbSession =
  Session.sql $
    "CREATE TABLE IF NOT EXISTS messages (id SERIAL PRIMARY KEY, content TEXT NOT NULL, created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP)"

-- Save message to database
storeMessageSession :: Text -> Session.Session ()
storeMessageSession msg = Session.statement msg storeMessageStatement

-- SQL statement for storing messages
storeMessageStatement :: HS.Statement Text ()
storeMessageStatement = HS.Statement sqlQuery encoder decoder True
 where
  sqlQuery = "INSERT INTO messages (content) VALUES ($1)"
  encoder = HE.param (HE.nonNullable HE.text)
  decoder = HD.noResult

-- Page with database access
mypage :: (Hyperbole :> es, DB :> es) => Eff es (Page '[Message])
mypage = do
  pure $ col id $ do
    hyper Message1 $ messageView "Hello"
    hyper Message2 $ messageView "World!"

data Message = Message1 | Message2
  deriving (Show, Read, ViewId)

instance (IOE :> es, DB :> es) => HyperView Message es where
  data Action Message = Louder Text
    deriving (Show, Read, ViewAction)

  update (Louder msg) = do
    -- Log when the action is executed
    liftIO $ logInfo $ "Making text louder: " ++ show msg

    -- Get database pool from Reader effect
    pool <- ask -- @Pool.Pool

    -- Store message in database using pool
    let logMsg = "Stored in DB: " <> msg
    dbResult <- liftIO $ Pool.use pool (storeMessageSession logMsg)

    -- Log database result
    case dbResult of
      Left err -> liftIO $ logInfo $ "DB Error: " ++ show err
      Right _ -> liftIO $ logInfo "Successfully stored message in database"

    -- Return updated view with louder text
    let new = msg <> "!"
    pure $ messageView new

messageView :: Text -> View Message ()
messageView msg = do
  row id $ do
    -- Log when button is clicked
    button (Louder msg) id "Louder"
    el_ $ text msg
