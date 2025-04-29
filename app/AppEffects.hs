{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}

module AppEffects 
  ( AppEffects
  , DB
  ) where

import Effectful
import Effectful.Reader.Static
import Config (ConfigEnv)
import Logger (LogEnv)

-- Import DB type but without creating a circular dependency
import qualified Hasql.Pool as Pool

-- For our DB effect (copied from DbPool to avoid circular deps)
type DB = Reader Pool.Pool

-- Common type alias for our application effects
type AppEffects es = (IOE :> es, DB :> es, ConfigEnv :> es, LogEnv :> es)