{-# LANGUAGE BlockArguments #-}
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

import Data.Int (Int32)
import Data.Text (Text)
import qualified Data.Text as Text
import Effectful
import Effectful.Reader.Static
import qualified Hasql.Pool as Pool
import Hasql.Session ()
import Logger (logInfo)
import Web.Hyperbole

-- Import our database pool module
import DbPool (
  DB,
  getRecentMessagesSession,
  storeMessageSession,
  withPool,
 )

main :: IO ()
main = do
  -- Log application startup
  logInfo "Starting Hyperbole application on port 3000"

  -- Create and use a connection pool with proper resource management
  withPool $ \pool -> do
    -- Run application with database connection pool
    logInfo "Starting application server on port 3000"
    run 3000 $ do
      liveApp
        (basicDocument "Example with Connection Pool")
        (runReader pool $ runPage mypage)

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
    hyper RecentMessages messagesView

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
    ( \(msgId, content, timestamp) -> do
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
