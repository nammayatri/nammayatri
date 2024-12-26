{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE StandaloneDeriving #-}

module SystemConfigsOverride where

import Data.Text as T
import qualified Database.Beam as B
import Kernel.Beam.Functions (FromTType' (fromTType'), ToTType' (toTType'), findOneWithDb)
import Kernel.Beam.Lib.UtilsTH
import Kernel.Prelude
import Kernel.Storage.Esqueleto.Config
import Kernel.Tools.Metrics.CoreMetrics.Types
import Kernel.Types.CacheFlow (CacheFlow)
import Sequelize as Se
import System.Environment (lookupEnv)
import System.IO.Unsafe (unsafePerformIO)

data SystemConfigsOverrridenD = SystemConfigsOverrridenD
  { idD :: Text,
    configValueD :: Text
  }
  deriving (Generic)

data SystemConfigsOverrridenT f = SystemConfigsOverrridenT
  { id :: B.C f Text,
    configValue :: B.C f Text
  }
  deriving (Generic, B.Beamable)

instance B.Table SystemConfigsOverrridenT where
  data PrimaryKey SystemConfigsOverrridenT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . id

type SystemConfigsOverrriden = SystemConfigsOverrridenT Identity

$(enableKVPG ''SystemConfigsOverrridenT ['id] [])

$(mkTableInstancesGenericSchema ''SystemConfigsOverrridenT "system_configs")

instance HasSchemaName SystemConfigsOverrridenT where
  schemaName _ = T.pack . fromMaybe "atlas_app" . unsafePerformIO $ lookupEnv "GET_MY_SCHEMA"

instance FromTType' SystemConfigsOverrriden SystemConfigsOverrridenD where
  fromTType' SystemConfigsOverrridenT {..} = do
    pure $ Just SystemConfigsOverrridenD {idD = id, configValueD = configValue}

instance ToTType' SystemConfigsOverrriden SystemConfigsOverrridenD where
  toTType' SystemConfigsOverrridenD {..} = do
    SystemConfigsOverrridenT {id = idD, configValue = configValueD}

findById :: (CacheFlow m r, EsqDBFlow m r, HasSchemaName SystemConfigsOverrridenT) => Text -> m (Maybe Text)
findById cfgId = do
  findOneWithDb [Se.Is id $ Se.Eq cfgId] <&> (<&> configValueD)
    >>= maybe (incrementSystemConfigsFailedCounter ("system_configs_find_failed_" <> schemaName (Proxy :: Proxy SystemConfigsOverrridenT) <> "_" <> cfgId) >> pure Nothing) (pure . Just)
