{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Beckn.Storage.DB.Config where

import Beckn.Types.Common
import Beckn.Types.Schema
import Beckn.Utils.Dhall (FromDhall)
import EulerHS.Prelude
import qualified EulerHS.Types as T
import GHC.Records (HasField (..))

data DBConfig = DBConfig
  { connTag :: T.ConnTag,
    pgConfig :: T.PostgresConfig,
    poolConfig :: T.PoolConfig,
    schemaName :: Text
  }
  deriving (Show, Generic, ToJSON, FromJSON, FromDhall)

-- Make the compiler generate instances for us!
type HasDbCfg r = (HasField "dbCfg" r DBConfig)

type FlowWithDb r a = HasDbCfg r => FlowR r a

instance HasDbCfg r => HasSchemaName (FlowR r) where
  getSchemaName =
    asks (schemaName <$> getField @"dbCfg")