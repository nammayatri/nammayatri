{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Types.DriverLocation where

import Data.Time
import qualified Domain.Types.Merchant as DMerchant
import qualified Domain.Types.MerchantOperatingCity as DMOC
import Domain.Types.Person (Person)
import Domain.Types.Ride (RideStatus)
import EulerHS.Prelude hiding (id, state)
import Kernel.External.Maps.HasCoordinates
import Kernel.Types.Id
import SharedLogic.External.LocationTrackingService.Types (RideInfo)

data RideDetails = RideDetails
  { rideId :: Text,
    rideStatus :: RideStatus,
    rideInfo :: Maybe RideInfo
  }
  deriving (Generic, Show, Eq, FromJSON, ToJSON)

data DriverLocation = DriverLocation
  { driverId :: Id Person,
    lat :: Double,
    lon :: Double,
    coordinatesCalculatedAt :: UTCTime,
    createdAt :: UTCTime,
    updatedAt :: UTCTime,
    merchantId :: Id DMerchant.Merchant,
    merchantOperatingCityId :: Maybe (Id DMOC.MerchantOperatingCity), -- Need to check that location service send mocid
    rideDetails :: Maybe RideDetails
  }
  deriving (Generic, Show, Eq, HasCoordinates, FromJSON, ToJSON)
