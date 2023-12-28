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

module Storage.Beam.Merchant.RiderConfig where

import qualified Database.Beam as B
import Database.Beam.MySQL ()
import GHC.Generics (Generic)
import Kernel.Prelude hiding (Generic)
import Tools.Beam.UtilsTH

data RiderConfigT f = RiderConfigT
  { merchantOperatingCityId :: B.C f Text,
    enableLocalPoliceSupport :: B.C f Bool,
    localPoliceNumber :: B.C f (Maybe Text),
    enableSupportForSafety :: B.C f Bool,
    videoFileSizeUpperLimit :: B.C f Int,
    createdAt :: B.C f UTCTime,
    updatedAt :: B.C f UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table RiderConfigT where
  data PrimaryKey RiderConfigT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . merchantOperatingCityId

type RiderConfig = RiderConfigT Identity

$(enableKVPG ''RiderConfigT ['merchantOperatingCityId] [])

$(mkTableInstances ''RiderConfigT "rider_config")
