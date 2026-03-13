{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.CorporateCommuteEvents
  ( CorporateTripEventType (..),
    CorporateTripEventPayload (..),
    CorporateBillingEventType (..),
    CorporateBillingEventPayload (..),
    CorporateSafetyEventType (..),
    CorporateSafetyEventPayload (..),
  )
where

import Kernel.Prelude
import qualified Kernel.Types.Common as Common

-- | Types of corporate trip events tracked to ClickHouse
data CorporateTripEventType
  = TRIP_SCHEDULED
  | TRIP_CONFIRMED
  | TRIP_DRIVER_ASSIGNED
  | TRIP_STARTED
  | TRIP_COMPLETED
  | TRIP_CANCELLED
  | TRIP_NO_SHOW
  deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

-- | Payload for corporate trip events
data CorporateTripEventPayload = CorporateTripEventPayload
  { corporateEntityId :: Text,
    corporateEmployeeId :: Text,
    shiftId :: Maybe Text,
    routeId :: Maybe Text,
    bookingId :: Maybe Text,
    rideId :: Maybe Text,
    pickupLat :: Maybe Double,
    pickupLon :: Maybe Double,
    dropLat :: Maybe Double,
    dropLon :: Maybe Double,
    estimatedFare :: Maybe Common.HighPrecMoney,
    actualFare :: Maybe Common.HighPrecMoney,
    currency :: Maybe Text,
    vehicleTier :: Maybe Text,
    distanceMeters :: Maybe Int,
    durationSeconds :: Maybe Int,
    scheduledPickupTime :: Maybe UTCTime,
    actualPickupTime :: Maybe UTCTime,
    actualDropTime :: Maybe UTCTime,
    status :: Maybe Text
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

-- | Types of corporate billing events tracked to ClickHouse
data CorporateBillingEventType
  = INVOICE_GENERATED
  | INVOICE_SENT
  | INVOICE_PAID
  | INVOICE_DISPUTED
  | WALLET_TOPPED_UP
  | WALLET_DEBITED
  | WALLET_FROZEN
  | CREDIT_NOTE_ISSUED
  deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

-- | Payload for corporate billing events
data CorporateBillingEventPayload = CorporateBillingEventPayload
  { corporateEntityId :: Text,
    invoiceId :: Maybe Text,
    billingModel :: Maybe Text,
    billingCycleType :: Maybe Text,
    amount :: Maybe Common.HighPrecMoney,
    taxAmount :: Maybe Common.HighPrecMoney,
    netAmount :: Maybe Common.HighPrecMoney,
    currency :: Maybe Text,
    tripCount :: Maybe Int,
    employeeCount :: Maybe Int,
    periodStart :: Maybe UTCTime,
    periodEnd :: Maybe UTCTime,
    walletBalanceBefore :: Maybe Common.HighPrecMoney,
    walletBalanceAfter :: Maybe Common.HighPrecMoney,
    paymentStatus :: Maybe Text
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

-- | Types of corporate safety events tracked to ClickHouse
data CorporateSafetyEventType
  = ROUTE_DEVIATION
  | SOS_TRIGGERED
  | NIGHT_RIDE_STARTED
  | WOMEN_SAFETY_ALERT
  | DRIVER_CHANGE
  | GEOFENCE_BREACH
  deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema)

-- | Payload for corporate safety events
data CorporateSafetyEventPayload = CorporateSafetyEventPayload
  { corporateEntityId :: Text,
    corporateEmployeeId :: Text,
    shiftId :: Maybe Text,
    routeId :: Maybe Text,
    bookingId :: Maybe Text,
    rideId :: Maybe Text,
    severity :: Maybe Text,
    isNightShift :: Maybe Bool,
    isWomenSafety :: Maybe Bool,
    deviationMeters :: Maybe Int,
    sosTriggered :: Maybe Bool,
    driverId :: Maybe Text,
    vehicleNumber :: Maybe Text,
    lat :: Maybe Double,
    lon :: Maybe Double,
    description :: Maybe Text
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)
