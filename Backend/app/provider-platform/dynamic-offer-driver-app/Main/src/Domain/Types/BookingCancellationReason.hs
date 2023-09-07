{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}

module Domain.Types.BookingCancellationReason where

import qualified Domain.Types.Booking as DRB
import Domain.Types.CancellationReason (CancellationReasonCode)
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Ride as DRide
import EulerHS.Prelude hiding (id)
import Kernel.External.Maps
import Kernel.Types.Common
import Kernel.Types.Id
import Tools.Beam.UtilsTH (mkBeamInstancesForEnum)

data BookingCancellationReason = BookingCancellationReason
  { driverId :: Maybe (Id DP.Person),
    bookingId :: Id DRB.Booking,
    rideId :: Maybe (Id DRide.Ride),
    merchantId :: Maybe (Id DM.Merchant),
    source :: CancellationSource,
    reasonCode :: Maybe CancellationReasonCode,
    additionalInfo :: Maybe Text,
    driverCancellationLocation :: Maybe LatLong,
    driverDistToPickup :: Maybe Meters
  }
  deriving (Generic)

data CancellationSource
  = ByUser
  | ByDriver
  | ByMerchant
  | ByAllocator
  | ByApplication
  deriving stock (Show, Eq, Read, Ord, Generic)
  deriving anyclass (FromJSON, ToJSON)

$(mkBeamInstancesForEnum ''CancellationSource)
