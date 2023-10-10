{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Types.FarePolicy.FarePolicyRentalSlabsDetails
  ( 
    module Reexport,
    module Domain.Types.FarePolicy.FarePolicyRentalSlabsDetails,
  )
where

import Domain.Types.Common
import Domain.Types.FarePolicy.Common as Reexport
import Kernel.Prelude
import Kernel.Types.Common

data FPRSlabDetailsD (s :: UsageSafety) = FPRSlabDetails
  { 
    id :: Text,
    baseDuration :: Int,
    baseDistance :: Kilometers,
    baseFare :: Money,
    kmAddedForEveryExtraHour :: Kilometers,
    extraRentalKmFare :: Money,
    extraRentalHoursFare :: Money,
    waitingChargeInfo :: Maybe WaitingChargeInfo,
    platformFeeInfo :: Maybe PlatformFeeInfo,
    nightShiftCharge :: Maybe NightShiftCharge
  }
  deriving (Generic, Show, Eq)

type FPRSlabDetails = FPRSlabDetailsD 'Safe

instance FromJSON (FPRSlabDetailsD 'Unsafe)

instance ToJSON (FPRSlabDetailsD 'Unsafe)

-----------------------------------------------------------------------------------------------------------------------------------------
------------------------------------------------APIEntity--------------------------------------------------------------------------------
-----------------------------------------------------------------------------------------------------------------------------------------

data FPRSlabDetailsAPIEntity = FPRSlabDetailsAPIEntity
  { baseDuration :: Int,
    baseDistance :: Kilometers,
    baseFare :: Money,
    kmAddedForEveryExtraHour :: Kilometers,
    extraRentalKmFare :: Money,
    extraRentalHoursFare :: Money,
    waitingChargeInfo :: Maybe WaitingChargeInfo,
    platformFeeInfo :: Maybe PlatformFeeInfo,
    nightShiftCharge :: Maybe NightShiftCharge
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

makeFPRSlabDetailsAPIEntity :: FPRSlabDetails -> FPRSlabDetailsAPIEntity
makeFPRSlabDetailsAPIEntity FPRSlabDetails {..} =
  FPRSlabDetailsAPIEntity
    { ..
    }
