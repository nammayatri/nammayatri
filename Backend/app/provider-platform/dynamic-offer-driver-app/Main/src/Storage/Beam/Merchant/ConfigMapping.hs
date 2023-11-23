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

module Storage.Beam.Merchant.ConfigMapping where

import qualified Database.Beam as B
import Domain.Types.Vehicle.Variant (Variant)
import Kernel.Prelude
import Kernel.Types.Common (Meters)
import Tools.Beam.UtilsTH

data ConfigMappingT f = ConfigMappingT
  { merchantId :: B.C f Text,
    merchantOperatingCityId :: B.C f Text,
    distance :: B.C f Meters,
    varType :: B.C f (Maybe Variant),
    startTime :: B.C f TimeOfDay,
    endTime :: B.C f TimeOfDay,
    tableName :: B.C f Text,
    configMapId :: B.C f Text,
    createdAt :: B.C f UTCTime,
    updatedAt :: B.C f UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table ConfigMappingT where
  data PrimaryKey ConfigMappingT f = CompositeKey (B.C f Text) (B.C f Meters) (B.C f (Maybe Variant)) (B.C f TimeOfDay) (B.C f TimeOfDay) (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey dt =
    CompositeKey
      (merchantOperatingCityId dt)
      (distance dt)
      (varType dt)
      (startTime dt)
      (endTime dt)
      (tableName dt)

type ConfigMapping = ConfigMappingT Identity

$(enableKVPG ''ConfigMappingT ['merchantOperatingCityId, 'distance, 'varType, 'startTime, 'endTime, 'tableName] [])

$(mkTableInstances ''ConfigMappingT "config_mapping")
