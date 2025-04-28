{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
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
import Logger (LogEnv, logDebug, logError, logInfo, textToPriority, withLoggerLevel)
import qualified System.Log.Logger as Log
import Web.Hyperbole

-- Import our database pool module
import DbPool (
  DB,
  getRecentMessagesSession,
  storeMessageSession,
  withConfiguredPool,
 )

-- Import our configuration module
import Config (
  AppConfig (..),
  ConfigEnv,
  defaultConfig,
  welcomeMessage,
  withConfig,
 )

main :: IO ()
main = do
  -- Start by initializing the hslogger
  Log.infoM "Main" "Starting Hyperbole application"

  -- Create a configuration with default values
  let config =
        defaultConfig
          { configAppName = "Hyperbole Demo App"
          , configWelcomeMessage = "Welcome to our amazing app!"
          , configLogLevel = "DEBUG" -- Set log level to DEBUG for development
          }

  -- Setup the logger with proper resource management, using log level from config
  withLoggerLevel "HyperboleApp" (textToPriority (configLogLevel config)) $ \logger -> do
    Log.infoM "Main" "Logger initialized"

    -- Use the configuration with proper resource management
    withConfig config $ \appConfig -> do
      -- Run the effectful application code with both config and logger in readers
      runEff $ runReader logger $ runReader appConfig $ do
        -- Get welcome message from config
        welcome <- welcomeMessage
        logInfo $ "Welcome message: " ++ Text.unpack welcome
        logDebug "Initializing application components"

        -- Create and use a connection pool with proper resource management using config
        withConfiguredPool $ \pool -> do
          -- Run application with database connection pool
          logInfo "Starting application server on port 3000"

          -- Get the app config for passing to the page
          appConf <- ask

          -- Run the web server with all three readers (config, pool, and logger)
          liftIO $ run 3000 $ do
            liveApp
              (basicDocument $ configAppName appConf)
              (runReader logger $ runReader appConf $ runReader pool $ runPage mypage)

-- Page with database access
mypage :: (Hyperbole :> es, DB :> es, ConfigEnv :> es, LogEnv :> es, IOE :> es) => Eff es (Page '[Message, RecentMessages])
mypage = do
  -- Get configuration from Reader effect
  welcome <- welcomeMessage

  -- Get database pool from Reader effect
  pool <- ask @Pool.Pool

  -- Get recent messages from database
  logDebug "Retrieving recent messages from database"
  messagesResult <- liftIO $ Pool.use pool getRecentMessagesSession

  -- Format messages or display error
  let messagesView = case messagesResult of
        Left err -> messagesErrorView (show err)
        Right messages -> recentMessagesView messages

  -- Create page with message input and recent messages display
  pure $ col id $ do
    -- Display the welcome message from config
    row id $ do
      el_ $ text welcome

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
            el_ $
              text $
                "#"
                  <> Text.pack (show msgId)
                  <> ": "
                  <> content
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

instance (IOE :> es, DB :> es, ConfigEnv :> es, LogEnv :> es) => HyperView Message es where
  data Action Message = Louder Text
    deriving (Show, Read, ViewAction)

  update (Louder msg) = do
    -- Get welcome message from config
    welcome <- welcomeMessage

    -- Log when the action is executed with welcome message
    logInfo $ "Making text louder: " ++ show msg ++ " (from " ++ Text.unpack welcome ++ ")"

    -- Get database pool from Reader effect
    pool <- ask @Pool.Pool

    -- Store message in database using pool
    let logMsg = "Stored in DB: " <> msg
    logDebug $ "Storing message in database: " ++ Text.unpack logMsg
    dbResult <- liftIO $ Pool.use pool (storeMessageSession logMsg)

    -- Log database result
    case dbResult of
      Left err -> do
        logError $ "DB Error: " ++ show err
        logDebug "The database operation failed"
      Right _ -> logInfo "Successfully stored message in database"

    -- Return updated view with louder text
    let new = msg <> "!"
    logDebug $ "Returning updated view with louder text: " ++ Text.unpack new
    pure $ messageView new

-- HyperView instance for RecentMessages
instance (IOE :> es, DB :> es, LogEnv :> es) => HyperView RecentMessages es where
  data Action RecentMessages = RefreshMessages
    deriving (Show, Read, ViewAction)

  update RefreshMessages = do
    -- Log refresh action
    logInfo "Refreshing recent messages"

    -- Get database pool from Reader effect
    pool <- ask

    -- Get recent messages from database
    logDebug "Fetching messages from database"
    messagesResult <- liftIO $ Pool.use pool getRecentMessagesSession

    -- Log result and return updated view
    case messagesResult of
      Left err -> do
        logError $ "Error retrieving messages: " ++ show err
        logDebug "Will display error view to user"
        pure $ messagesErrorView (show err)
      Right msgs -> do
        logInfo $ "Retrieved " ++ show (length msgs) ++ " messages"
        logDebug "Will display messages to user"
        pure $ recentMessagesView msgs

messageView :: Text -> View Message ()
messageView m = do
  row (gap 10) $ do
    button (Louder m) (border 1 . pad 5) "Louder"
    el (pad 5) $ text m
