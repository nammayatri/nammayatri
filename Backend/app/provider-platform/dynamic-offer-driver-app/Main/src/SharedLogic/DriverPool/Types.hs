{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.DriverPool.Types
  ( DriverPoolResult (..),
    DriverPoolResultCurrentlyOnRide (..),
    DriverPoolWithActualDistResult (..),
    PoolRadiusStep,
    PoolBatchNum,
  )
where

import Domain.Types.Person (Driver)
import qualified Domain.Types.Vehicle as Vehicle
import EulerHS.Prelude hiding (id)
import qualified Kernel.External.FCM.Types as FCM
import qualified Kernel.External.Maps as Maps
import Kernel.Types.Id
import Kernel.Utils.Common
import Tools.Maps as Google

type PoolBatchNum = Int

type PoolRadiusStep = Int

data DriverPoolResult = DriverPoolResult
  { driverId :: Id Driver,
    language :: Maybe Maps.Language,
    driverDeviceToken :: Maybe FCM.FCMRecipientToken,
    distanceToPickup :: Meters,
    -- durationToPickup :: Seconds,
    variant :: Vehicle.Variant,
    lat :: Double,
    lon :: Double
  }
  deriving (Generic, Show, HasCoordinates, FromJSON, ToJSON)

data DriverPoolResultCurrentlyOnRide = DriverPoolResultCurrentlyOnRide
  { driverId :: Id Driver,
    language :: Maybe Maps.Language,
    driverDeviceToken :: Maybe FCM.FCMRecipientToken,
    variant :: Vehicle.Variant,
    lat :: Double,
    lon :: Double,
    destinationLat :: Double,
    destinationLon :: Double,
    distanceToPickup :: Meters
  }
  deriving (Generic, Show, HasCoordinates, FromJSON, ToJSON)

data DriverPoolWithActualDistResult = DriverPoolWithActualDistResult
  { driverPoolResult :: DriverPoolResult,
    actualDistanceToPickup :: Meters,
    actualDurationToPickup :: Seconds,
    rideRequestPopupDelayDuration :: Seconds,
    cancellationRatio :: Maybe Double,
    acceptanceRatio :: Maybe Double,
    driverAvailableTime :: Maybe Double,
    isPartOfIntelligentPool :: Bool
  }
  deriving (Generic, Show, FromJSON, ToJSON)

instance HasCoordinates DriverPoolWithActualDistResult where
  getCoordinates r = getCoordinates r.driverPoolResult
