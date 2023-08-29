{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Beam.RegistryMapFallback where

import Data.Serialize
import qualified Database.Beam as B
import Database.Beam.MySQL ()
import EulerHS.KVConnector.Types (KVConnector (..), MeshMeta (..), primaryKey, secondaryKeys, tableName)
import GHC.Generics (Generic)
import Kernel.Beam.Lib.UtilsTH
import Kernel.Prelude hiding (Generic)
import Lib.Utils ()
import Sequelize

data RegistryMapFallbackT f = RegistryMapFallbackT
  { subscriberId :: B.C f Text,
    uniqueId :: B.C f Text,
    registryUrl :: B.C f Text
  }
  deriving (Generic, B.Beamable)

instance B.Table RegistryMapFallbackT where
  data PrimaryKey RegistryMapFallbackT f
    = CompositeKey (B.C f Text) (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = CompositeKey <$> subscriberId <*> uniqueId

type RegistryMapFallback = RegistryMapFallbackT Identity

registryMapFallbackTMod :: RegistryMapFallbackT (B.FieldModification (B.TableField RegistryMapFallbackT))
registryMapFallbackTMod =
  B.tableModification
    { subscriberId = B.fieldNamed "subscriber_id",
      uniqueId = B.fieldNamed "unique_id",
      registryUrl = B.fieldNamed "registry_url"
    }

$(enableKVPG ''RegistryMapFallbackT ['subscriberId, 'uniqueId] [])

$(mkTableInstances ''RegistryMapFallbackT "registry_map_fallback" "atlas_driver_offer_bpp")
