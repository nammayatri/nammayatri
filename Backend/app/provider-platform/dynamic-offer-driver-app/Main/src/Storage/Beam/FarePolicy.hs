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

module Storage.Beam.FarePolicy where

import qualified Database.Beam as B
import qualified Domain.Types.FarePolicy as Domain
import Kernel.Prelude
import Kernel.Types.Common hiding (id)
import Tools.Beam.UtilsTH

data FarePolicyT f = FarePolicyT
  { id :: B.C f Text,
    farePolicyType :: B.C f Domain.FarePolicyType,
    serviceCharge :: B.C f (Maybe Money),
    nightShiftStart :: B.C f (Maybe TimeOfDay),
    nightShiftEnd :: B.C f (Maybe TimeOfDay),
    maxAllowedTripDistance :: B.C f (Maybe Meters),
    minAllowedTripDistance :: B.C f (Maybe Meters),
    govtCharges :: B.C f (Maybe Double),
    description :: B.C f (Maybe Text),
    createdAt :: B.C f UTCTime,
    updatedAt :: B.C f UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table FarePolicyT where
  data PrimaryKey FarePolicyT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . id

type FarePolicy = FarePolicyT Identity

$(enableKVPG ''FarePolicyT ['id] [[]])

$(mkTableInstances ''FarePolicyT "fare_policy")
