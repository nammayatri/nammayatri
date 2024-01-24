{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TemplateHaskell #-}

module Dashboard.ProviderPlatform.Volunteer
  ( module Dashboard.ProviderPlatform.Volunteer,
    module Reexport,
  )
where

import Dashboard.Common as Reexport
import Data.Aeson
import Data.OpenApi hiding (description, name, password, summary, title, url)
import Data.Text as T
import Kernel.External.Maps.Types
import Kernel.Prelude
import Kernel.Storage.Esqueleto (derivePersistField)
import Kernel.Types.APISuccess (APISuccess)
import Kernel.Types.Common
import Kernel.Types.Id
import Servant hiding (Summary)

data VolunteerEndpoint
  = AssignCreateAndStartOtpRideEndpoint
  deriving (Show, Read)

derivePersistField "VolunteerEndpoint"

-- Booking Info
--
type BookingInfoAPI =
  Capture "bookingOtp" Text
    :> "booking"
    :> Get '[JSON] BookingInfoResponse

data BookingInfoResponse = BookingInfoResponse
  { bookingId :: Id Booking,
    fromLocation :: Location,
    toLocation :: Maybe Location,
    estimatedDistance :: Maybe Meters,
    estimatedFare :: Money,
    estimatedDuration :: Maybe Seconds,
    riderName :: Maybe Text,
    vehicleVariant :: Variant
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data Location = Location
  { id :: Id Location,
    lat :: Double,
    lon :: Double,
    address :: LocationAddress,
    createdAt :: UTCTime,
    updatedAt :: UTCTime
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data LocationAddress = LocationAddress
  { street :: Maybe Text,
    city :: Maybe Text,
    state :: Maybe Text,
    country :: Maybe Text,
    building :: Maybe Text,
    areaCode :: Maybe Text,
    area :: Maybe Text
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

-- AssignCreateAndStartOtpRideAPI
type AssignCreateAndStartOtpRideAPI =
  "assign"
    :> "start"
    :> ReqBody '[JSON] AssignCreateAndStartOtpRideAPIReq
    :> Post '[JSON] APISuccess

data AssignCreateAndStartOtpRideAPIReq = AssignCreateAndStartOtpRideAPIReq
  { bookingId :: Id Booking,
    driverId :: Id Driver,
    point :: LatLong
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance HideSecrets AssignCreateAndStartOtpRideAPIReq where
  hideSecrets = identity
