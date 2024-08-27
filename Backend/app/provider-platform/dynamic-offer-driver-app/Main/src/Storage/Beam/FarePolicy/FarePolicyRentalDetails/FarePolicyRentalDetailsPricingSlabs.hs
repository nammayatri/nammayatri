{-
  Copyright 2022-23, Juspay India Pvt Ltd
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}

module Storage.Beam.FarePolicy.FarePolicyRentalDetails.FarePolicyRentalDetailsPricingSlabs where

import qualified Database.Beam as B
import qualified Domain.Types.FarePolicy as Domain
import Domain.Types.UtilsTH
import Kernel.Prelude
import qualified Kernel.Types.Id as KTI
import Tools.Beam.UtilsTH

data FarePolicyRentalDetailsPricingSlabsT f = FarePolicyRentalDetailsPricingSlabsT
  { id :: B.C f (Maybe Int),
    farePolicyId :: B.C f Text,
    timePercentage :: B.C f Int,
    distancePercentage :: B.C f Int,
    farePercentage :: B.C f Int,
    includeActualTimePercentage :: B.C f Bool,
    includeActualDistPercentage :: B.C f Bool
  }
  deriving (Generic, B.Beamable)

instance B.Table FarePolicyRentalDetailsPricingSlabsT where
  data PrimaryKey FarePolicyRentalDetailsPricingSlabsT f
    = Id (B.C f (Maybe Int))
    deriving (Generic, B.Beamable)
  primaryKey = Id . id

type FarePolicyRentalDetailsPricingSlabs = FarePolicyRentalDetailsPricingSlabsT Identity

type FullFarePolicyRentalDetailsPricingSlabs = (KTI.Id Domain.FarePolicy, Domain.FPRentalDetailsPricingSlabs)

$(enableKVPG ''FarePolicyRentalDetailsPricingSlabsT ['id] [['farePolicyId]])

$(mkTableInstances ''FarePolicyRentalDetailsPricingSlabsT "fare_policy_rental_details_pricing_slabs")
$(mkCacParseInstanceList ''FarePolicyRentalDetailsPricingSlabs)
