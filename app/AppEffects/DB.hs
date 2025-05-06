{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

{- | Database effect module
This module provides the DB effect type and functions
-}
module AppEffects.DB (
  -- * Database Effect
  DB,

  -- * Session runner
  runSession,
) where

import Effectful
import Effectful.Reader.Static
import qualified Hasql.Pool as Pool
import qualified Hasql.Session as Session

-- | DB effect (a reader effect for the database pool)
type DB = Reader Pool.Pool

-- | Run a database session using the pool from the Reader effect
runSession :: (DB :> es, IOE :> es) => Session.Session a -> Eff es (Either Pool.UsageError a)
runSession session = do
  pool <- ask @Pool.Pool
  liftIO $ Pool.use pool session
