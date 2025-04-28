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
import Effectful
import Effectful.Reader.Static
import Hasql.Connection (Connection)
import qualified Hasql.Connection as Connection
import qualified Hasql.Decoders as HD
import qualified Hasql.Encoders as HE
import qualified Hasql.Session as Session
import qualified Hasql.Statement as HS
import Logger (logInfo)
import Web.Hyperbole

-- Effect for database access, review, Petter3
type DB = Reader Connection

main :: IO ()
main = do
  -- Log application startup
  logInfo "Starting Hyperbole application on port 3000"

  -- Connect to PostgreSQL database
  -- Empty list for default settings (localhost)
  connResult <- Connection.acquire []

  case connResult of
    Left err -> do
      logInfo $ "Database connection error: " ++ show err
      error "Failed to connect to the database"
    Right conn -> do
      logInfo "Successfully connected to PostgreSQL database"

      -- Setup database table if needed
      setupDbResult <- Session.run setupDbSession conn
      case setupDbResult of
        Left err -> logInfo $ "Database setup error: " ++ show err
        Right _ -> logInfo "Database ready"

      -- Run application with database connection
      run 3000 $ do
        liveApp
          (basicDocument "Example")
          (runReader conn $ runPage mypage)

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

    -- Get database connection from Reader effect
    conn <- ask -- @Connection

    -- Store message in database
    let logMsg = "Stored in DB: " <> msg
    dbResult <- liftIO $ Session.run (storeMessageSession logMsg) conn

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
