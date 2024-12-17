{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is

  distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS

  FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of the GNU Affero

  General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.External.LocationTrackingService.Types where

import Domain.Types.Common (DriverMode)
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Ride as DRide
import Domain.Types.VehicleVariant (VehicleVariant)
import Kernel.External.Maps.Types
import Kernel.Prelude
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.Common
import Kernel.Utils.Dhall (FromDhall)

type LTSFlow m r c = (CoreMetrics m, MonadFlow m, HasFlowEnv m r '["ltsCfg" ::: LocationTrackingeServiceConfig], HasShortDurationRetryCfg r c)

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
    driverId :: Id DP.Person,
    nextRideId :: Maybe (Id DRide.Ride)
  }
  deriving (Generic, FromJSON, ToJSON, ToSchema)

data NearByReq = NearByReq
  { lat :: Double,
    lon :: Double,
    onRide :: Maybe Bool,
    vehicleType :: Maybe [VehicleVariant],
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
    driverMode :: DriverMode
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema, Show)

data RideDetailsReq = RideDetailsReq
  { rideId :: Id DRide.Ride,
    rideStatus :: DRide.RideStatus,
    merchantId :: Id DM.Merchant,
    driverId :: Id DP.Person,
    isFutureRide :: Maybe Bool,
    lat :: Double,
    lon :: Double
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema, Show)

newtype DriversLocationReq = DriversLocationReq
  { driverIds :: [Id DP.Person]
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema, Show)

data DriverLocationReq = DriverLocationReq
  { driverId :: Id DP.Person,
    merchantId :: Id DM.Merchant
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema, Show)

newtype DriverLocationResp = DriverLocationResp
  { loc :: [LatLong]
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema, Show)

type HasLocationService m r = (HasFlowEnv m r '["ltsCfg" ::: LocationTrackingeServiceConfig])

data DriverBlockTillReq = DriverBlockTillReq
  { merchantId :: Id DM.Merchant,
    driverId :: Id DP.Person,
    blockTill :: UTCTime
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema, Show)
