{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Beckn.Storage.DB.Config where

import Beckn.Types.App (MonadFlow)
import Beckn.Types.Flow
import Beckn.Types.Schema
import Beckn.Utils.Dhall (FromDhall)
import EulerHS.Prelude
import qualified EulerHS.Types as T
import GHC.Records.Extra (HasField (..))

data DBConfig = DBConfig
  { connTag :: T.ConnTag,
    pgConfig :: T.PostgresConfig,
    poolConfig :: T.PoolConfig,
    schemaName :: Text
  }
  deriving (Show, Generic, ToJSON, FromJSON, FromDhall)

-- Make the compiler generate instances for us!
type HasDbCfg r = (HasField "dbCfg" r DBConfig)

type HasFlowDBEnv m r =
  ( MonadReader r m,
    HasField "dbCfg" r DBConfig,
    MonadFlow m,
    HasSchemaName m
  )

instance HasDbCfg r => HasSchemaName (FlowR r) where
  getSchemaName =
    asks (.dbCfg.schemaName)
