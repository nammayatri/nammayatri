{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}

module Storage.Beam.RegistryMapFallback where

import qualified Data.Time as Time
import qualified Database.Beam as B
import Kernel.Prelude
import Tools.Beam.UtilsTH

data RegistryMapFallbackT f = RegistryMapFallbackT
  { subscriberId :: B.C f Text,
    uniqueId :: B.C f Text,
    registryUrl :: B.C f Text,
    updatedAt :: B.C f Time.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table RegistryMapFallbackT where
  data PrimaryKey RegistryMapFallbackT f
    = CompositeKey (B.C f Text) (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = CompositeKey <$> subscriberId <*> uniqueId

type RegistryMapFallback = RegistryMapFallbackT Identity

$(enableKVPG ''RegistryMapFallbackT ['subscriberId, 'uniqueId] [])

$(mkTableInstances ''RegistryMapFallbackT "registry_map_fallback")
