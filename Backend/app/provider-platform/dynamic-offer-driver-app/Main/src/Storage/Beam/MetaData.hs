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

module Storage.Beam.MetaData where

import qualified Database.Beam as B
import Kernel.Prelude
import Tools.Beam.UtilsTH

data MetaDataT f = MetaDataT
  { driverId :: B.C f Text,
    device :: B.C f (Maybe Text),
    deviceOS :: B.C f (Maybe Text),
    deviceDateTime :: B.C f (Maybe UTCTime),
    appPermissions :: B.C f (Maybe Text),
    createdAt :: B.C f UTCTime,
    updatedAt :: B.C f UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table MetaDataT where
  data PrimaryKey MetaDataT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . driverId

type MetaData = MetaDataT Identity

$(enableKVPG ''MetaDataT ['driverId] [])

$(mkTableInstancesWithTModifier ''MetaDataT "meta_data" [("deviceOS", "device_o_s")])
