{-
  Copyright 2022-23, Juspay India Pvt Ltd
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Storage.Beam.FarePolicy.FarePolicyRentalDetails.FarePolicyRentalDetailsPricingSlabs where

import qualified Database.Beam as B
import qualified Domain.Types.FarePolicy as Domain
import Domain.Types.UtilsTH
import Kernel.Prelude
import qualified Kernel.Types.Id as KTI
import Tools.Beam.UtilsTH

data FarePolicyRentalDetailsPricingSlabsT f = FarePolicyRentalDetailsPricingSlabsT
  { farePolicyId :: B.C f Text,
    timePercentage :: B.C f Int,
    distancePercentage :: B.C f Int,
    farePercentage :: B.C f Int,
    includeActualTimePercentage :: B.C f Bool,
    includeActualDistPercentage :: B.C f Bool
  }
  deriving (Generic, B.Beamable)

instance B.Table FarePolicyRentalDetailsPricingSlabsT where
  data PrimaryKey FarePolicyRentalDetailsPricingSlabsT f
    = FarePolicyRentalDetailsPricingSlabsKey (B.C f Text) (B.C f Int) (B.C f Int)
    deriving (Generic, B.Beamable)
  primaryKey FarePolicyRentalDetailsPricingSlabsT {..} = FarePolicyRentalDetailsPricingSlabsKey farePolicyId timePercentage distancePercentage

type FarePolicyRentalDetailsPricingSlabs = FarePolicyRentalDetailsPricingSlabsT Identity

type FullFarePolicyRentalDetailsPricingSlabs = (KTI.Id Domain.FarePolicy, Domain.FPRentalDetailsPricingSlabs)

$(enableKVPG ''FarePolicyRentalDetailsPricingSlabsT ['farePolicyId, 'timePercentage, 'distancePercentage] [['farePolicyId]])

$(mkTableInstances ''FarePolicyRentalDetailsPricingSlabsT "fare_policy_rental_details_pricing_slabs")
$(mkCacParseInstanceList ''FarePolicyRentalDetailsPricingSlabs)
