{-
  Copyright 2022-23, Juspay India Pvt Ltd
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}

module Storage.Beam.FarePolicy.FarePolicyRentalDetails where

import qualified "dashboard-helper-api" Dashboard.ProviderPlatform.Merchant as DPM
import qualified Database.Beam as B
import qualified Domain.Types.FarePolicy as Domain
import Kernel.Prelude
import Kernel.Types.Common hiding (id)
import qualified Kernel.Types.Id as KTI
import Tools.Beam.UtilsTH

data FarePolicyRentalDetailsT f = FarePolicyRentalDetailsT
  { farePolicyId :: B.C f Text,
    baseFare :: B.C f Money,
    perHourCharge :: B.C f Money,
    perHourFreeKms :: B.C f Int,
    perExtraKmRate :: B.C f Money,
    nightShiftCharge :: B.C f (Maybe DPM.NightShiftCharge)
  }
  deriving (Generic, B.Beamable)

instance B.Table FarePolicyRentalDetailsT where
  data PrimaryKey FarePolicyRentalDetailsT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . farePolicyId

type FarePolicyRentalDetails = FarePolicyRentalDetailsT Identity

type FullFarePolicyRentalDetails = (KTI.Id Domain.FarePolicy, Domain.FPRentalDetails)

$(enableKVPG ''FarePolicyRentalDetailsT ['farePolicyId] [])

$(mkTableInstances ''FarePolicyRentalDetailsT "fare_policy_rental_details")
