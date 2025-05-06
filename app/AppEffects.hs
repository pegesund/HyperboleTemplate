{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}

module AppEffects 
  ( -- * Application effects
    AppEffects
  -- * Re-export effects
  , module AppEffects.DB
  , module AppEffects.Config
  , module AppEffects.Logger
  ) where

import Effectful
import AppEffects.Config (Config, AppConfig(..), defaultConfig, withConfig, welcomeMessage)
import AppEffects.Logger (Logger, logDebug, logError, logInfo, withLoggerLevel, textToPriority)
import AppEffects.DB

-- Common type alias for our application effects
type AppEffects es = (IOE :> es, DB :> es, Config :> es, Logger :> es)