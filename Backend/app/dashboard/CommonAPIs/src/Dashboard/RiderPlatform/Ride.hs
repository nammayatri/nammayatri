{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingVia #-}

module Dashboard.RiderPlatform.Ride
  ( module Dashboard.RiderPlatform.Ride,
    module Reexport,
  )
where

import Dashboard.Common as Reexport
import qualified Dashboard.Common as DP
import Kernel.External.Maps
import Kernel.Prelude
import Kernel.Types.Centesimal
import Kernel.Types.Id
import Servant

---------------------------------------------------------
-- share ride info--------------------------------------

type ShareRideInfoAPI =
  Capture "rideId" (Id DP.Ride)
    :> "info"
    :> Get '[JSON] ShareRideInfoRes

data ShareRideInfoRes = ShareRideInfoRes
  { id :: Id Ride,
    bookingId :: Id Booking,
    status :: RideStatus,
    driverName :: Text,
    driverRating :: Maybe Centesimal,
    vehicleNumber :: Text,
    vehicleModel :: Text,
    vehicleColor :: Text,
    trackingUrl :: Maybe BaseUrl,
    rideStartTime :: Maybe UTCTime,
    rideEndTime :: Maybe UTCTime,
    userFirstName :: Maybe Text,
    userLastName :: Maybe Text,
    fromLocation :: BookingLocation,
    toLocation :: Maybe BookingLocation
  }
  deriving (Generic, Show, ToSchema, FromJSON, ToJSON)

data RideStatus
  = NEW
  | INPROGRESS
  | COMPLETED
  | CANCELLED
  deriving (Show, Eq, Ord, Read, Generic, ToJSON, FromJSON, ToSchema)

data BookingLocation = BookingLocation
  { id :: Id BookingLocation,
    lat :: Double,
    lon :: Double,
    address :: LocationAddress,
    createdAt :: UTCTime,
    updatedAt :: UTCTime
  }
  deriving (Generic, Show, Eq, HasCoordinates, ToSchema, FromJSON, ToJSON)

data LocationAddress = LocationAddress
  { street :: Maybe Text,
    city :: Maybe Text,
    state :: Maybe Text,
    country :: Maybe Text,
    building :: Maybe Text,
    areaCode :: Maybe Text,
    area :: Maybe Text
  }
  deriving (Generic, Show, Eq, ToSchema, FromJSON, ToJSON)
