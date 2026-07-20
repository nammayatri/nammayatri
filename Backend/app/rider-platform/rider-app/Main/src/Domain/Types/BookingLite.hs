{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Types.BookingLite where

import Domain.Types.Booking (Booking)
import qualified Domain.Types.BookingStatus as DBS
import qualified Domain.Types.Common as DTC
import qualified Domain.Types.Ride as DR
import qualified Domain.Types.ServiceTierType as DVST
import Kernel.Prelude
import Kernel.Types.Common (HighPrecMoney)
import Kernel.Types.Id
import qualified SharedLogic.Type as SLT

data BookingLiteRow = BookingLiteRow
  { bookingId :: Id Booking,
    status :: DBS.BookingStatus,
    startTime :: UTCTime, -- rideScheduledTime + merge ordering
    createdAt :: UTCTime,
    isScheduled :: Maybe Bool,
    serviceTierName :: Maybe Text,
    vehicleServiceTierType :: DVST.ServiceTierType,
    vehicleIconUrl :: Maybe Text,
    isAirConditioned :: Maybe Bool,
    tripCategory :: Maybe DTC.TripCategory,
    billingCategory :: Maybe SLT.BillingCategory,
    estimatedFare :: HighPrecMoney,
    fromLocationId :: Maybe Text,
    toLocationId :: Maybe Text,
    locationNames :: Maybe [Text]
  }

data RideLiteRow = RideLiteRow
  { rideId :: Id DR.Ride,
    bookingId :: Id Booking,
    totalFare :: Maybe HighPrecMoney, -- app's "computedPrice"
    cancellationChargesOnCancel :: Maybe HighPrecMoney,
    cancellationFeeStatus :: Maybe DR.CancellationFeeStatus
  }
