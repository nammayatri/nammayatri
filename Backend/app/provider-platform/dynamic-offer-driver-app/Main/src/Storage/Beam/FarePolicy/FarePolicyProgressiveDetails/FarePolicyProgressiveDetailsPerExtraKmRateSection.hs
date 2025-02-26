{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Storage.Beam.FarePolicy.FarePolicyProgressiveDetails.FarePolicyProgressiveDetailsPerExtraKmRateSection where

import qualified Database.Beam as B
import qualified Domain.Types.FarePolicy as Domain
import Domain.Types.UtilsTH
import Kernel.Prelude
import Kernel.Types.Common hiding (id)
import qualified Kernel.Types.Id as KTI
import Tools.Beam.UtilsTH

data FarePolicyProgressiveDetailsPerExtraKmRateSectionT f = FarePolicyProgressiveDetailsPerExtraKmRateSectionT
  { -- id :: B.C f Text,
    farePolicyId :: B.C f Text,
    startDistance :: B.C f Meters,
    distanceUnit :: B.C f (Maybe DistanceUnit),
    baseFareDepreciation :: B.C f (Maybe HighPrecMoney),
    perExtraKmRate :: B.C f HighPrecMoney
  }
  deriving (Generic, B.Beamable)

instance B.Table FarePolicyProgressiveDetailsPerExtraKmRateSectionT where
  data PrimaryKey FarePolicyProgressiveDetailsPerExtraKmRateSectionT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . farePolicyId

type FarePolicyProgressiveDetailsPerExtraKmRateSection = FarePolicyProgressiveDetailsPerExtraKmRateSectionT Identity

type FullFarePolicyProgressiveDetailsPerExtraKmRateSection = (KTI.Id Domain.FarePolicy, Domain.FPProgressiveDetailsPerExtraKmRateSection)

$(enableKVPG ''FarePolicyProgressiveDetailsPerExtraKmRateSectionT ['farePolicyId] [])

$(mkTableInstances ''FarePolicyProgressiveDetailsPerExtraKmRateSectionT "fare_policy_progressive_details_per_extra_km_rate_section")
$(mkCacParseInstanceList ''FarePolicyProgressiveDetailsPerExtraKmRateSection)
