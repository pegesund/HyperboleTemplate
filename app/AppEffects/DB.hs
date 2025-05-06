{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}

-- | Database effect module
-- This module provides the DB effect type and functions
module AppEffects.DB
  ( -- * Database Effect
    DB
    -- * Session runner
  , runSession
    -- * Effect runners
  , runAppEffects
  , runHyperbolePage
  ) where

import Effectful
import Effectful.Reader.Static
import qualified Hasql.Pool as Pool
import qualified Hasql.Session as Session

import AppEffects.Config (AppConfig, Config, runConfig)
import AppEffects.Logger (Logger, runLogger)
import qualified Effectful.Reader.Static as Reader
import Web.Hyperbole (Hyperbole)

-- | DB effect (a reader effect for the database pool)
type DB = Reader Pool.Pool

-- | Run a database session using the pool from the Reader effect
runSession :: (DB :> es, IOE :> es) => Session.Session a -> Eff es (Either Pool.UsageError a)
runSession session = do
  pool <- ask @Pool.Pool
  liftIO $ Pool.use pool session

-- | Run all application effects in one go
runAppEffects ::
  -- | Application configuration
  AppConfig ->
  -- | Logger name
  String ->
  -- | Database connection pool
  Pool.Pool ->
  -- | Action to run with all effects available
  Eff '[DB, Config, Logger, IOE] a ->
  -- | Resulting IO action
  IO a
runAppEffects config loggerName pool action =
  runEff $                         -- Run IOE
    runLogger loggerName $         -- Run Logger
      runConfig config $           -- Run Config
        Reader.runReader pool action      -- Run DB

-- | Prepare a Hyperbole page with all our application effects
-- (Note: This doesn't actually run the Hyperbole effect, it just prepares it for runPage)
runHyperbolePage ::
  -- | Application configuration
  AppConfig ->
  -- | Logger name
  String ->
  -- | Database connection pool
  Pool.Pool ->
  -- | Hyperbole page with application effects
  Eff '[DB, Config, Logger, Hyperbole, IOE] a ->
  -- | Resulting action with only Hyperbole effect remaining
  Eff '[Hyperbole, IOE] a
runHyperbolePage config loggerName pool action =
  runLogger loggerName $         -- Run Logger
    runConfig config $           -- Run Config
      Reader.runReader pool action      -- Run DB