{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}

module Storage.Beam.FarePolicy.DriverExtraFeeBounds where

import qualified Database.Beam as B
import Kernel.Prelude
import Kernel.Types.Common hiding (id)
import Tools.Beam.UtilsTH

data DriverExtraFeeBoundsT f = DriverExtraFeeBoundsT
  { id :: B.C f (Maybe Int),
    farePolicyId :: B.C f Text,
    startDistance :: B.C f Meters,
    minFee :: B.C f Money,
    maxFee :: B.C f Money
  }
  deriving (Generic, B.Beamable)

instance B.Table DriverExtraFeeBoundsT where
  data PrimaryKey DriverExtraFeeBoundsT f
    = Id (B.C f (Maybe Int))
    deriving (Generic, B.Beamable)
  primaryKey = Id . id

type DriverExtraFeeBounds = DriverExtraFeeBoundsT Identity

$(enableKVPG ''DriverExtraFeeBoundsT ['id] [['farePolicyId]])

$(mkTableInstances ''DriverExtraFeeBoundsT "fare_policy_driver_extra_fee_bounds")
