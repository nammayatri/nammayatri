module SharedLogic.DriverPool.Types
  ( DriverPoolResult (..),
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
    parallelSearchRequestCount :: Int,
    -- durationToPickup :: Seconds,
    variant :: Vehicle.Variant,
    lat :: Double,
    lon :: Double
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
