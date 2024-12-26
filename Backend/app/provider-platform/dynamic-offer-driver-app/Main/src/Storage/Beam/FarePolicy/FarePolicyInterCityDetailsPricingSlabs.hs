{-
  Copyright 2022-23, Juspay India Pvt Ltd
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Storage.Beam.FarePolicy.FarePolicyInterCityDetailsPricingSlabs where

import qualified Database.Beam as B
import qualified Domain.Types.FarePolicy as Domain
import qualified Domain.Types.FarePolicy.FarePolicyInterCityDetailsPricingSlabs as Domain
import Domain.Types.UtilsTH
import Kernel.Prelude
import qualified Kernel.Types.Id as KTI
import Tools.Beam.UtilsTH

data FarePolicyInterCityDetailsPricingSlabsT f = FarePolicyInterCityDetailsPricingSlabsT
  { farePolicyId :: B.C f Text,
    timePercentage :: B.C f Int,
    distancePercentage :: B.C f Int,
    farePercentage :: B.C f Int,
    includeActualTimePercentage :: B.C f Bool,
    includeActualDistPercentage :: B.C f Bool
  }
  deriving (Generic, B.Beamable)

instance B.Table FarePolicyInterCityDetailsPricingSlabsT where
  data PrimaryKey FarePolicyInterCityDetailsPricingSlabsT f
    = FarePolicyInterCityDetailsPricingSlabsKey (B.C f Text) (B.C f Int) (B.C f Int)
    deriving (Generic, B.Beamable)
  primaryKey FarePolicyInterCityDetailsPricingSlabsT {..} = FarePolicyInterCityDetailsPricingSlabsKey farePolicyId timePercentage distancePercentage

type FarePolicyInterCityDetailsPricingSlabs = FarePolicyInterCityDetailsPricingSlabsT Identity

type FullFarePolicyInterCityDetailsPricingSlabs = (KTI.Id Domain.FarePolicy, Domain.FPInterCityDetailsPricingSlabs)

$(enableKVPG ''FarePolicyInterCityDetailsPricingSlabsT ['farePolicyId, 'timePercentage, 'distancePercentage] [['farePolicyId]])

$(mkTableInstances ''FarePolicyInterCityDetailsPricingSlabsT "fare_policy_inter_city_details_pricing_slabs")
$(mkCacParseInstanceList ''FarePolicyInterCityDetailsPricingSlabs)
