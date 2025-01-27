{-
  Copyright 2022-23, Juspay India Pvt Ltd
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Storage.Beam.FarePolicy.FarePolicyAmbulanceDetailsSlab where

import qualified Database.Beam as B
import qualified Domain.Types.FarePolicy as Domain
import Domain.Types.UtilsTH
import Kernel.Prelude
import Kernel.Types.Common hiding (id)
import qualified Kernel.Types.Id as KTI
import Tools.Beam.UtilsTH

data FarePolicyAmbulanceDetailsSlabT f = FarePolicyAmbulanceDetailsSlabT
  { id :: B.C f Int,
    farePolicyId :: B.C f Text,
    baseFare :: B.C f HighPrecMoney,
    baseDistance :: B.C f Meters,
    perKmRate :: B.C f HighPrecMoney,
    currency :: B.C f Currency,
    vehicleAge :: B.C f Months,
    platformFeeCharge :: B.C f (Maybe Domain.PlatformFeeCharge),
    platformFeeCgst :: B.C f (Maybe Double),
    platformFeeSgst :: B.C f (Maybe Double),
    waitingCharge :: B.C f (Maybe Domain.WaitingCharge),
    freeWaitingTime :: B.C f (Maybe Minutes),
    nightShiftCharge :: B.C f (Maybe Domain.NightShiftCharge)
  }
  deriving (Generic, B.Beamable)

instance B.Table FarePolicyAmbulanceDetailsSlabT where
  data PrimaryKey FarePolicyAmbulanceDetailsSlabT f
    = Id (B.C f Int)
    deriving (Generic, B.Beamable)
  primaryKey = Id . id

type FarePolicyAmbulanceDetailsSlab = FarePolicyAmbulanceDetailsSlabT Identity

type FullFarePolicyAmbulanceDetailsSlab = (KTI.Id Domain.FarePolicy, Domain.FPAmbulanceDetailsSlab)

$(enableKVPG ''FarePolicyAmbulanceDetailsSlabT ['id] [['farePolicyId]])

$(mkTableInstances ''FarePolicyAmbulanceDetailsSlabT "fare_policy_ambulance_details_slab")

$(mkCacParseInstance ''FarePolicyAmbulanceDetailsSlabT)
