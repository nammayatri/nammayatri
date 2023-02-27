{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE UndecidableInstances #-}

module Domain.Types.RideRequest where

import Data.OpenApi hiding (info)
import Data.Time (UTCTime)
import qualified Domain.Types.Booking as DRB
import Domain.Types.Merchant
import Domain.Types.Person (Driver)
import EulerHS.Prelude hiding (id)
import Kernel.Types.Id
import Kernel.Utils.GenericPretty

data RideRequestType = ALLOCATION | CANCELLATION | DRIVER_RESPONSE
  deriving (Show, Eq, Read, Generic, ToJSON, FromJSON)
  deriving (PrettyShow) via Showable RideRequestType

data RideRequest = RideRequest
  { id :: Id RideRequest,
    bookingId :: Id DRB.Booking,
    subscriberId :: ShortId Subscriber,
    createdAt :: UTCTime,
    _type :: RideRequestType,
    info :: Maybe DriverResponse
  }
  deriving (Generic, Eq, Show, ToJSON, FromJSON, PrettyShow)

data DriverResponse = DriverResponse
  { driverId :: Id Driver,
    status :: NotificationStatus
  }
  deriving (Show, Generic, Eq, FromJSON, ToJSON, ToSchema, PrettyShow)

data NotificationStatus
  = ACCEPT
  | REJECT
  deriving (Show, Generic, Eq, ToJSON, FromJSON, ToSchema)
  deriving (PrettyShow) via Showable NotificationStatus
