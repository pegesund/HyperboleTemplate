{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Main (main) where

import Data.Int (Int32)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Control.Monad (forM_)
import Effectful
import Effectful.Reader.Static
import qualified System.Log.Logger as Log
import qualified Hasql.Pool as Pool
import Web.Hyperbole

-- Import effects and runners from AppEffects
import AppEffects
import AppEffects.Config (runConfig)
import AppEffects.Logger (runLogger)

-- Import application-specific database functionality
import DbActions (getRecentMessagesSession, storeMessageSession, initPoolWithConfig)

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
      -- We'll set up database connection directly, without using the effect system
      Log.infoM "Main" "Initializing database connection pool"
      
      -- Create a connection pool directly using the configuration
      let connStr = "host=" <> T.unpack (configDbHost appConfig) <> 
                    " dbname=" <> T.unpack (configDbName appConfig) <> 
                    " user=" <> T.unpack (configDbUser appConfig) <> 
                    " password=" <> T.unpack (configDbPassword appConfig)
      
      -- Log configuration details for debugging
      Log.debugM "Main" $ "Database connection: host=" ++ T.unpack (configDbHost appConfig) 
                          ++ " dbname=" ++ T.unpack (configDbName appConfig)
                          ++ " user=" ++ T.unpack (configDbUser appConfig)
                          ++ " (password hidden)"
      Log.debugM "Main" $ "Pool configuration: size=" ++ show (configPoolSize appConfig)
                         ++ ", timeout=10s, lifetime=600s, idletime=600s"
      
      -- Initialize the pool directly
      pool <- initPoolWithConfig 
                (TE.encodeUtf8 $ T.pack connStr) 
                (configPoolSize appConfig) 
                10 -- acquisition timeout
                600 -- lifetime 
                600 -- idle time
      
      -- Log welcome message directly from config
      Log.infoM "Main" $ "Welcome message: " ++ T.unpack (configWelcomeMessage appConfig)
      Log.debugM "Main" "Initializing application components"
      Log.infoM "Main" "Starting application server on port 3000"
      
      -- Start the web server directly
      run 3000 $ do
          -- Set up the Hyperbole effect stack just for the page rendering
          -- This is the only place we actually need effects
          let pageWithEffects = runConfig appConfig $ runLogger logger $ runReader pool $ runPage mypage
          
          -- Create the Hyperbole web application
          liveApp
              (basicDocument $ configAppName appConfig)
              pageWithEffects
              
      -- Clean up resources
      Log.infoM "Main" "Shutting down database pool"
      Pool.release pool

-- Page with database access
mypage :: (Hyperbole :> es, AppEffects es) => Eff es (Page '[Message, RecentMessages])
mypage = do
  -- Get configuration from Reader effect
  welcome <- welcomeMessage

  -- Get recent messages from database using our DB effect
  logDebug "Retrieving recent messages from database"
  messagesResult <- runSession getRecentMessagesSession

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
  forM_
    messages
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

-- View for when there's an error loading messages
messagesErrorView :: String -> View RecentMessages ()
messagesErrorView errorMsg = do
  row id $ do
    el_ $ text "Error loading messages from database"
    button RefreshMessages id "Try Again"
  row id $ do
    el_ $ text $ Text.pack errorMsg

instance (AppEffects es) => HyperView Message es where
  data Action Message = Louder Text
    deriving (Show, Read, ViewAction)

  update (Louder msg) = do
    -- Get welcome message from config
    welcome <- welcomeMessage

    -- Log when the action is executed with welcome message
    logInfo $ "Making text louder: " ++ show msg ++ " (from " ++ Text.unpack welcome ++ ")"

    -- Store message in database using our DB effect
    let logMsg = "Stored in DB: " <> msg
    logDebug $ "Storing message in database: " ++ Text.unpack logMsg
    dbResult <- runSession (storeMessageSession logMsg)

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
instance (AppEffects es) => HyperView RecentMessages es where
  data Action RecentMessages = RefreshMessages
    deriving (Show, Read, ViewAction)

  update RefreshMessages = do
    -- Log refresh action
    logInfo "Refreshing recent messages"

    -- Get recent messages from database using our DB effect
    logDebug "Fetching messages from database"
    messagesResult <- runSession getRecentMessagesSession

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
