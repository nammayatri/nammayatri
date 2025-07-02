{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Types.FarePolicy.FarePolicySlabsDetails.FarePolicySlabsDetailsSlab where

import Data.Aeson as DA
import Domain.Types.Common
import Domain.Types.FarePolicy.Common as DFPC
import Kernel.Prelude as KP
import Kernel.Types.Common
import Tools.Beam.UtilsTH (mkBeamInstancesForJSON)

data FPSlabsDetailsSlabD (s :: UsageSafety) = FPSlabsDetailsSlab
  { startDistance :: Meters,
    distanceUnit :: DistanceUnit,
    baseFare :: HighPrecMoney,
    waitingChargeInfo :: Maybe DFPC.WaitingChargeInfo,
    platformFeeInfo :: Maybe PlatformFeeInfo,
    nightShiftCharge :: Maybe DFPC.NightShiftCharge,
    currency :: Currency
  }
  deriving (Generic, Show, Eq, ToSchema)

type FPSlabsDetailsSlab = FPSlabsDetailsSlabD 'Safe

instance FromJSON (FPSlabsDetailsSlabD 'Unsafe)

instance ToJSON (FPSlabsDetailsSlabD 'Unsafe)

-- FIXME remove
instance FromJSON (FPSlabsDetailsSlabD 'Safe)

-- FIXME remove
instance ToJSON (FPSlabsDetailsSlabD 'Safe)

data PlatformFeeCharge = ProgressivePlatformFee HighPrecMoney | ConstantPlatformFee HighPrecMoney
  deriving stock (Show, Eq, Read, Ord, Generic)
  deriving anyclass (FromJSON, ToJSON, ToSchema)

data PlatformFeeInfo = PlatformFeeInfo
  { platformFeeCharge :: PlatformFeeCharge,
    cgst :: Double,
    sgst :: Double
  }
  deriving (Generic, Eq, Show, ToJSON, FromJSON, ToSchema)

$(mkBeamInstancesForJSON ''PlatformFeeCharge)
