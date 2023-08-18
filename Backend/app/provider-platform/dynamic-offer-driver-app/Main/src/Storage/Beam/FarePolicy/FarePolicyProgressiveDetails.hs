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

module Storage.Beam.FarePolicy.FarePolicyProgressiveDetails where

import Data.Serialize
import qualified Database.Beam as B
import Database.Beam.MySQL ()
import qualified Domain.Types.FarePolicy
import qualified Domain.Types.FarePolicy as Domain
import qualified Domain.Types.Vehicle.Variant as Vehicle
import EulerHS.KVConnector.Types (KVConnector (..), MeshMeta (..), primaryKey, secondaryKeys, tableName)
import GHC.Generics (Generic)
import Kernel.Beam.Lib.UtilsTH
import Kernel.Prelude hiding (Generic)
import Kernel.Types.Common hiding (id)
import Lib.Utils ()
import Sequelize as Se

instance IsString Vehicle.Variant where
  fromString = show

data FarePolicyProgressiveDetailsT f = FarePolicyProgressiveDetailsT
  { farePolicyId :: B.C f Text,
    baseDistance :: B.C f Meters,
    baseFare :: B.C f Money,
    deadKmFare :: B.C f Money,
    waitingCharge :: B.C f (Maybe Domain.WaitingCharge),
    freeWatingTime :: B.C f (Maybe Minutes),
    nightShiftCharge :: B.C f (Maybe Domain.NightShiftCharge)
  }
  deriving (Generic, B.Beamable)

instance B.Table FarePolicyProgressiveDetailsT where
  data PrimaryKey FarePolicyProgressiveDetailsT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . farePolicyId

type FarePolicyProgressiveDetails = FarePolicyProgressiveDetailsT Identity

deriving stock instance Ord Domain.WaitingCharge

deriving stock instance Ord Domain.NightShiftCharge

farePolicyProgressiveDetailsTMod :: FarePolicyProgressiveDetailsT (B.FieldModification (B.TableField FarePolicyProgressiveDetailsT))
farePolicyProgressiveDetailsTMod =
  B.tableModification
    { farePolicyId = B.fieldNamed "fare_policy_id",
      baseDistance = B.fieldNamed "base_distance",
      baseFare = B.fieldNamed "base_fare",
      deadKmFare = B.fieldNamed "dead_km_fare",
      waitingCharge = B.fieldNamed "waiting_charge",
      freeWatingTime = B.fieldNamed "free_wating_time",
      nightShiftCharge = B.fieldNamed "night_shift_charge"
    }

$(enableKVPG ''FarePolicyProgressiveDetailsT ['farePolicyId] [])

$(mkTableInstances ''FarePolicyProgressiveDetailsT "fare_policy_progressive_details" "atlas_driver_offer_bpp")
