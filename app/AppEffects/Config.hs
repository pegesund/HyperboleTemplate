{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module AppEffects.Config
  ( -- * Config Effect
    Config(..)
  , runConfig
  , withConfig
    -- * Config Data
  , AppConfig(..)
  , defaultConfig
    -- * Config Access Functions
  , getConfig
  , welcomeMessage
  , getDbHost
  , getDbName
  , getDbUser
  , getDbPassword
  , getPoolSize
  , getLogLevel
  , buildConnectionString
  ) where

import Control.Exception (bracket)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.Text (Text)
import qualified Data.Text as T
import Effectful
import Effectful.Dispatch.Dynamic
import GHC.Generics (Generic)
import qualified System.Log.Logger as Log

-- | The configuration struct
data AppConfig = AppConfig
  { configAppName :: !Text
  , configWelcomeMessage :: !Text
  , configDbHost :: !Text
  , configDbName :: !Text
  , configDbUser :: !Text
  , configDbPassword :: !Text
  , configPoolSize :: !Int
  , configAcquisitionTimeout :: !Int -- seconds
  , configMaxLifetime :: !Int -- seconds
  , configMaxIdletime :: !Int -- seconds
  , configLogLevel :: !Text -- DEBUG, INFO, WARNING, ERROR, CRITICAL
  }
  deriving (Show, Generic)

-- | Default configuration
defaultConfig :: AppConfig
defaultConfig =
  AppConfig
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
    , configLogLevel = "INFO" -- Default to INFO level
    }

-- | The Config effect
data Config :: Effect where
  GetAppConfig :: Config m AppConfig

type instance DispatchOf Config = 'Dynamic

-- | Get the current configuration
getConfig :: (Config :> es) => Eff es AppConfig
getConfig = send GetAppConfig

-- | Run the Config effect with a specified configuration
runConfig :: AppConfig -> Eff (Config : es) a -> Eff es a
runConfig config = interpret $ \_ -> \case
  GetAppConfig -> pure config

-- | Helper to initialize and clean up config resources
withConfig :: AppConfig -> (AppConfig -> IO a) -> IO a
withConfig config action = do
  Log.infoM "Config" $ "Initializing application with config: " ++ T.unpack (configAppName config)
  bracket
    (pure config)
    ( \_ -> do
        Log.infoM "Config" "Config resources cleaned up"
    )
    action

-- | Helper function to construct a connection string from config
buildConnectionString :: AppConfig -> ByteString
buildConnectionString config =
  BS.pack $
    "host=" ++ T.unpack (configDbHost config)
      ++ " dbname="
      ++ T.unpack (configDbName config)
      ++ " user="
      ++ T.unpack (configDbUser config)
      ++ " password="
      ++ T.unpack (configDbPassword config)

-- Accessor functions to get specific config values
welcomeMessage :: (Config :> es) => Eff es Text
welcomeMessage = configWelcomeMessage <$> getConfig

getDbHost :: (Config :> es) => Eff es Text
getDbHost = configDbHost <$> getConfig

getDbName :: (Config :> es) => Eff es Text
getDbName = configDbName <$> getConfig

getDbUser :: (Config :> es) => Eff es Text
getDbUser = configDbUser <$> getConfig

getDbPassword :: (Config :> es) => Eff es Text
getDbPassword = configDbPassword <$> getConfig

getPoolSize :: (Config :> es) => Eff es Int
getPoolSize = configPoolSize <$> getConfig

getLogLevel :: (Config :> es) => Eff es Text
getLogLevel = configLogLevel <$> getConfig