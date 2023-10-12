{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}

module Storage.Beam.FarePolicy.FarePolicyRentalSlabsDetails where

-- import Data.Serialize
import qualified Database.Beam as B
import Database.Beam.Backend ()
import Database.Beam.MySQL ()
import qualified Domain.Types.FarePolicy as Domain
-- import EulerHS.KVConnector.Types (KVConnector (..), MeshMeta (..), primaryKey, secondaryKeys, tableName)
import GHC.Generics (Generic)
import Kernel.Prelude hiding (Generic)
import Kernel.Types.Common hiding (id)
import qualified Kernel.Types.Id as KTI
-- import Lib.Utils ()
-- import Sequelize as Se
import Tools.Beam.UtilsTH

data FarePolicyRentalSlabDetailsT f = FarePolicyRentalSlabDetailsT
  { id :: B.C f Text,
    farePolicyId :: B.C f Text,
    baseDistance :: B.C f Kilometers,
    baseDuration :: B.C f Seconds,
    kmAddedForEveryExtraHour :: B.C f Kilometers,
    extraRentalKmFare :: B.C f Money,
    extraRentalHoursFare :: B.C f Money,
    baseFare :: B.C f Money,
    platformFeeCharge :: B.C f (Maybe Domain.PlatformFeeCharge),
    platformFeeCgst :: B.C f (Maybe Double),
    platformFeeSgst :: B.C f (Maybe Double),
    waitingCharge :: B.C f (Maybe Domain.WaitingCharge),
    nightShiftCharge :: B.C f (Maybe Domain.NightShiftCharge),
    freeWatingTime :: B.C f (Maybe Minutes)
  }
  deriving (Generic, B.Beamable)

instance B.Table FarePolicyRentalSlabDetailsT where
  data PrimaryKey FarePolicyRentalSlabDetailsT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . id

type FarePolicyRentalSlabDetails = FarePolicyRentalSlabDetailsT Identity

type FullFarePolicyRentalSlabDetails = (KTI.Id Domain.FarePolicy, Domain.FPRSlabDetailsSlab)

$(enableKVPG ''FarePolicyRentalSlabDetailsT ['id] [['farePolicyId]])

$(mkTableInstances ''FarePolicyRentalSlabDetailsT "fare_policy_rental_slabs_details_slab")
