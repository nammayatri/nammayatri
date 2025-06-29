{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-incomplete-record-updates #-}

module Lib.DriverCoins.Types
  ( DriverCoinsEventType (..),
    DCoins.DriverCoinsFunctionType (..),
    DCoins.CoinMessage (..),
    DCoins.CoinStatus (..),
    DCoins.MetroRideType (..),
    DCoins.isMetroRideType,
    CancellationType (..),
  )
where

import qualified "dashboard-helper-api" API.Types.ProviderPlatform.Management.DriverCoins as DCoins
import Domain.Types.CancellationReason
import Domain.Types.Ride
import Kernel.Prelude
import Kernel.Types.Common (Meters)
import qualified Text.Show (show)

data CancellationType
  = CancellationByCustomer
  | CancellationByDriver
  deriving (Generic, ToJSON, FromJSON)

data DriverCoinsEventType
  = Rating {ratingValue :: Int, ride :: Ride}
  | EndRide {isDisabled :: Bool, coinsRewardedOnGoldTierRide :: Maybe Int, ride :: Ride, metroRideType :: DCoins.MetroRideType}
  | Cancellation {rideStartTime :: UTCTime, intialDisToPickup :: Maybe Meters, cancellationDisToPickup :: Maybe Meters, cancelledBy :: CancellationType, cancellationReason :: CancellationReasonCode}
  | DriverToCustomerReferral {ride :: Ride}
  | CustomerToDriverReferral
  | LeaderBoard
  | Training
  | BulkUploadEvent
  | LMS
  | LMSBonus
  deriving (Generic, ToJSON, FromJSON)

driverCoinsEventTypeToString :: DriverCoinsEventType -> String
driverCoinsEventTypeToString = \case
  Rating {} -> "Rating"
  EndRide {} -> "EndRide"
  Cancellation {} -> "Cancellation"
  DriverToCustomerReferral {} -> "DriverToCustomerReferral"
  CustomerToDriverReferral {} -> "CustomerToDriverReferral"
  LeaderBoard -> "LeaderBoard"
  Training -> "Training"
  BulkUploadEvent -> "BulkUploadEvent"
  LMS -> "LMS"
  LMSBonus -> "LMSBonus"

instance Show CancellationType where
  show CancellationByCustomer = "CancellationByCustomer"
  show CancellationByDriver = "CancellationByDriver"

instance Show DriverCoinsEventType where
  show = driverCoinsEventTypeToString
