{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.External.LocationTrackingService.Types where

import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Ride as DRide
import Kernel.External.Maps.Types
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Dhall (FromDhall)

data StartRideReq = StartRideReq
  { lat :: Double,
    lon :: Double,
    merchant_id :: Id DM.Merchant,
    driver_id :: Id DP.Person
  }
  deriving (Generic, FromJSON, ToJSON, ToSchema)

data EndRideReq = EndRideReq
  { lat :: Double,
    lon :: Double,
    merchant_id :: Id DM.Merchant,
    driver_id :: Id DP.Person
  }
  deriving (Generic, FromJSON, ToJSON, ToSchema)

data NearByReq = NearByReq
  { lat :: Double,
    lon :: Double,
    vehicle_type :: Text,
    radius :: Int,
    merchant_id :: Id DM.Merchant
  }
  deriving (Generic, FromJSON, ToJSON, ToSchema)

newtype LocationTrackingeServiceConfig = LocationTrackingeServiceConfig
  { url :: BaseUrl
  }
  deriving (Generic, FromJSON, ToJSON, Show, Eq, FromDhall)

data EndRideRes = EndRideRes
  { rideId :: Id DRide.Ride,
    driverId :: Id DP.Person,
    loc :: [LatLong]
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema, Show)
