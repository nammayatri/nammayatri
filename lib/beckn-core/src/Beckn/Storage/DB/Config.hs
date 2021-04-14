{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Beckn.Storage.DB.Config where

import Beckn.Types.Common
import Beckn.Types.Schema
import qualified Beckn.Types.Storage.ExternalTrail as ExternalTrail
import qualified Beckn.Types.Storage.Trail as Trail
import Beckn.Utils.Dhall (FromDhall)
import qualified Database.Beam as B
import qualified Database.Beam.Schema.Tables as B
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

data TrailDb f = TrailDb
  { _trail :: f (B.TableEntity Trail.TrailT),
    _externalTrail :: f (B.TableEntity ExternalTrail.ExternalTrailT)
  }
  deriving (Generic, B.Database be)

trailDb :: Text -> B.DatabaseSettings be TrailDb
trailDb dbSchemaName =
  B.defaultDbSettings
    `B.withDbModification` B.dbModification
      { _trail = setSchema dbSchemaName <> Trail.fieldEMod,
        _externalTrail = setSchema dbSchemaName <> ExternalTrail.fieldEMod
      }
  where
    setSchema x = setEntitySchema (Just x)
    -- FIXME: this is in beam > 0.8.0.0, and can be removed when we upgrade
    -- (introduced in beam commit id 4e3539784c4a0d58eea08129edd0dc094b0e9695)
    modifyEntitySchema modSchema =
      B.EntityModification (Endo (\(B.DatabaseEntity tbl) -> B.DatabaseEntity (tbl & B.dbEntitySchema %~ modSchema)))
    setEntitySchema nm = modifyEntitySchema (const nm)
