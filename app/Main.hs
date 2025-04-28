{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Main where

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TE
import Data.ByteString (ByteString)
import Data.Int (Int32)
import qualified Data.Vector as Vector
import Effectful
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

-- Page with database access
mypage :: (Hyperbole :> es, DB :> es, IOE :> es) => Eff es (Page '[Message, RecentMessages])
mypage = do
  -- Get database pool from Reader effect
  pool <- ask
  
  -- Get recent messages from database
  messagesResult <- liftIO $ Pool.use pool getRecentMessagesSession
  
  -- Format messages or display error
  let messagesView = case messagesResult of
        Left err -> messagesErrorView (show err)
        Right messages -> recentMessagesView messages
  
  -- Create page with message input and recent messages display
  pure $ col id $ do
    hyper Message1 $ messageView "Hello"
    hyper Message2 $ messageView "World!"
    hyper RecentMessages $ messagesView

data Message = Message1 | Message2
  deriving (Show, Read, ViewId)
  
-- Data type for displaying recent messages
data RecentMessages = RecentMessages
  deriving (Show, Read, ViewId)
  
-- View functions for displaying recent messages
recentMessagesView :: [(Int32, Text, Text)] -> View RecentMessages ()
recentMessagesView messages = do
  row id $ do
    el_ $ do
      text "Recent messages:"
    button RefreshMessages id "Refresh"
  
  -- Display each message
  mapM_ 
    (\(msgId, content, timestamp) -> do
      row id $ do
        col id $ do
          el_ $ text $ "#" <> (Text.pack $ show msgId) <> ": " <> content
          el_ $ text $ "Time: " <> timestamp
    ) 
    messages

-- View for when there's an error loading messages
messagesErrorView :: String -> View RecentMessages ()
messagesErrorView errorMsg = do
  row id $ do
    el_ $ text "Error loading messages from database"
    button RefreshMessages id "Try Again"
  row id $ do
    el_ $ text $ Text.pack errorMsg

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
    
-- HyperView instance for RecentMessages
instance (IOE :> es, DB :> es) => HyperView RecentMessages es where
  data Action RecentMessages = RefreshMessages
    deriving (Show, Read, ViewAction)
    
  update RefreshMessages = do
    -- Log refresh action
    liftIO $ logInfo "Refreshing recent messages"
    
    -- Get database pool from Reader effect
    pool <- ask
    
    -- Get recent messages from database
    messagesResult <- liftIO $ Pool.use pool getRecentMessagesSession
    
    -- Log result and return updated view
    case messagesResult of
      Left err -> do
        liftIO $ logInfo $ "Error retrieving messages: " ++ show err
        pure $ messagesErrorView (show err)
      Right msgs -> do
        liftIO $ logInfo $ "Retrieved " ++ show (length msgs) ++ " messages"
        pure $ recentMessagesView msgs

messageView :: Text -> View Message ()
messageView msg = do
  row id $ do
    -- Log when button is clicked
    button (Louder msg) id "Louder"
    el_ $ text msg
