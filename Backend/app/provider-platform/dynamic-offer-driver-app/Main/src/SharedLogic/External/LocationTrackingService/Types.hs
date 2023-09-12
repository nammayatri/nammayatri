{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.External.LocationTrackingService.Types where

import qualified Domain.Types.DriverInformation as DI
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Ride as DRide
import Domain.Types.Vehicle.Variant (Variant)
import Kernel.External.Maps.Types
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.Common
import Kernel.Utils.Dhall (FromDhall)

data StartRideReq = StartRideReq
  { lat :: Double,
    lon :: Double,
    merchantId :: Id DM.Merchant,
    driverId :: Id DP.Person
  }
  deriving (Generic, FromJSON, ToJSON, ToSchema)

data EndRideReq = EndRideReq
  { lat :: Double,
    lon :: Double,
    merchantId :: Id DM.Merchant,
    driverId :: Id DP.Person
  }
  deriving (Generic, FromJSON, ToJSON, ToSchema)

data NearByReq = NearByReq
  { lat :: Double,
    lon :: Double,
    onRide :: Maybe Bool,
    vehicleType :: Maybe Variant,
    radius :: Int,
    merchantId :: Id DM.Merchant
  }
  deriving (Generic, FromJSON, ToJSON, ToSchema)

newtype LocationTrackingeServiceConfig = LocationTrackingeServiceConfig
  { url :: BaseUrl
  }
  deriving (Generic, FromJSON, ToJSON, Show, Eq, FromDhall)

data EndRideRes = EndRideRes
  { rideId :: Id DRide.Ride,
    driverId :: Id DP.Person,
    loc :: NonEmpty LatLong
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema, Show)

data DriverDetailsReq = DriverDetailsReq
  { driverId :: Id DP.Person,
    driverMode :: DI.DriverMode
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema, Show)

data RideDetailsReq = RideDetailsReq
  { rideId :: Id DRide.Ride,
    rideStatus :: DRide.RideStatus,
    merchantId :: Id DM.Merchant,
    driverId :: Id DP.Person,
    lat :: Double,
    lon :: Double
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema, Show)

newtype DriversLocationReq = DriversLocationReq
  { driverIds :: [Id DP.Person]
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema, Show)

type HasLocationService m r = (HasField "enableLocationTrackingService" r Bool, HasFlowEnv m r '["ltsCfg" ::: LocationTrackingeServiceConfig])
