{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}

module Config 
  ( AppConfig(..)
  , defaultConfig
  , ConfigEnv
  , withConfig
  , getConfig
  , welcomeMessage
  , getDbHost
  , getDbName
  , getDbUser
  , getDbPassword
  , getPoolSize
  , withPoolConfig
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Effectful
import Effectful.Reader.Static
import Logger (logInfo)
import Control.Exception (bracket)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import GHC.Generics (Generic)

-- The configuration struct
data AppConfig = AppConfig 
  { configAppName           :: !Text
  , configWelcomeMessage    :: !Text
  , configDbHost            :: !Text
  , configDbName            :: !Text
  , configDbUser            :: !Text
  , configDbPassword        :: !Text
  , configPoolSize          :: !Int
  , configAcquisitionTimeout :: !Int  -- seconds
  , configMaxLifetime       :: !Int   -- seconds
  , configMaxIdletime       :: !Int   -- seconds
  } deriving (Show, Generic)

-- Default configuration
defaultConfig :: AppConfig
defaultConfig = AppConfig
  { configAppName = "Hyperbole App"
  , configWelcomeMessage = "Welcome to Hyperbole App!"
  , configDbHost = "localhost"
  , configDbName = "hyperbole"
  , configDbUser = "hyperbole"
  , configDbPassword = "hyperbole"
  , configPoolSize = 10
  , configAcquisitionTimeout = 10
  , configMaxLifetime = 600
  , configMaxIdletime = 600
  }

-- Effect type for accessing configuration
type ConfigEnv = Reader AppConfig

-- Helper to initialize and clean up config resources
withConfig :: AppConfig -> (AppConfig -> IO a) -> IO a
withConfig config action = do
  logInfo $ "Initializing application with config: " ++ T.unpack (configAppName config)
  bracket 
    (pure config) 
    (\_ -> logInfo "Config resources cleaned up") 
    action

-- Get the complete config from the reader monad
getConfig :: (ConfigEnv :> es) => Eff es AppConfig
getConfig = ask

-- Accessor functions to get specific config values
welcomeMessage :: (ConfigEnv :> es) => Eff es Text
welcomeMessage = configWelcomeMessage <$> ask

getDbHost :: (ConfigEnv :> es) => Eff es Text
getDbHost = configDbHost <$> ask

getDbName :: (ConfigEnv :> es) => Eff es Text
getDbName = configDbName <$> ask

getDbUser :: (ConfigEnv :> es) => Eff es Text
getDbUser = configDbUser <$> ask

getDbPassword :: (ConfigEnv :> es) => Eff es Text
getDbPassword = configDbPassword <$> ask

getPoolSize :: (ConfigEnv :> es) => Eff es Int
getPoolSize = configPoolSize <$> ask

-- Helper function to construct a connection string from config
buildConnectionString :: AppConfig -> ByteString
buildConnectionString config = BS.pack $ 
  "host=" ++ T.unpack (configDbHost config) ++ 
  " dbname=" ++ T.unpack (configDbName config) ++ 
  " user=" ++ T.unpack (configDbUser config) ++ 
  " password=" ++ T.unpack (configDbPassword config)

-- Execute an action with pool config derived from AppConfig
withPoolConfig :: (ConfigEnv :> es, IOE :> es) => 
                 (ByteString -> Int -> Int -> Int -> Int -> Eff es a) -> 
                 Eff es a
withPoolConfig action = do
  config <- ask
  let connString = buildConnectionString config
  action 
    connString 
    (configPoolSize config)
    (configAcquisitionTimeout config)
    (configMaxLifetime config)
    (configMaxIdletime config)