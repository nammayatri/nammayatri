{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.DriverPool.Types
  ( PoolCalculationStage (..),
    CalculateGoHomeDriverPoolReq (..),
    DriverPoolResult (..),
    DriverPoolResultCurrentlyOnRide (..),
    DriverPoolWithActualDistResult (..),
    DriverPoolWithActualDistResultWithFlags (..),
    PoolType (..),
    PoolRadiusStep,
    PoolBatchNum,
    castServiceTierToVariant,
    castVariantToServiceTier,
  )
where

import qualified Domain.Types.Driver.GoHomeFeature.DriverGoHomeRequest as DDGR
import qualified Domain.Types.DriverInformation as DI
import Domain.Types.DriverPoolConfig (DriverPoolConfig)
import Domain.Types.GoHomeConfig (GoHomeConfig)
import qualified Domain.Types.Merchant as DM
import Domain.Types.Merchant.DriverIntelligentPoolConfig (IntelligentScores)
import Domain.Types.Person (Driver)
import qualified Domain.Types.ServiceTierType as DVST
import qualified Domain.Types.Vehicle as Vehicle
import EulerHS.Prelude hiding (id)
import qualified Kernel.External.Maps as Maps
import qualified Kernel.External.Notification.FCM.Types as FCM
import Kernel.Types.Id
import Kernel.Types.Version
import Kernel.Utils.Common
import Tools.Maps as Google

type PoolBatchNum = Int

type PoolRadiusStep = Meters

data PoolCalculationStage = Estimate | DriverSelection

data PoolType = NormalPool | GoHomePool | SpecialDriversPool | SpecialZoneQueuePool | SkipPool deriving (Ord, Eq, Show)

data CalculateGoHomeDriverPoolReq a = CalculateGoHomeDriverPoolReq
  { poolStage :: PoolCalculationStage,
    driverPoolCfg :: DriverPoolConfig,
    goHomeCfg :: GoHomeConfig,
    serviceTier :: Maybe DVST.ServiceTierType,
    fromLocation :: a,
    toLocation :: a,
    merchantId :: Id DM.Merchant,
    isRental :: Bool
  }

data DriverPoolResult = DriverPoolResult
  { driverId :: Id Driver,
    language :: Maybe Maps.Language,
    driverDeviceToken :: Maybe FCM.FCMRecipientToken,
    distanceToPickup :: Meters,
    -- durationToPickup :: Seconds,
    variant :: Vehicle.Variant,
    serviceTier :: DVST.ServiceTierType,
    airConditioned :: Maybe Double,
    lat :: Double,
    lon :: Double,
    mode :: Maybe DI.DriverMode,
    clientSdkVersion :: Maybe Version,
    clientBundleVersion :: Maybe Version,
    clientConfigVersion :: Maybe Version,
    clientDevice :: Maybe Device,
    backendConfigVersion :: Maybe Version,
    backendAppVersion :: Maybe Text
  }
  deriving (Generic, Show, HasCoordinates, FromJSON, ToJSON)

data DriverPoolResultCurrentlyOnRide = DriverPoolResultCurrentlyOnRide
  { driverId :: Id Driver,
    language :: Maybe Maps.Language,
    driverDeviceToken :: Maybe FCM.FCMRecipientToken,
    variant :: Vehicle.Variant,
    serviceTier :: DVST.ServiceTierType,
    airConditioned :: Maybe Double,
    lat :: Double,
    lon :: Double,
    destinationLat :: Double,
    destinationLon :: Double,
    distanceToPickup :: Meters,
    distanceFromDriverToDestination :: Meters,
    mode :: Maybe DI.DriverMode,
    clientSdkVersion :: Maybe Version,
    clientBundleVersion :: Maybe Version,
    clientConfigVersion :: Maybe Version,
    clientDevice :: Maybe Device,
    backendConfigVersion :: Maybe Version,
    backendAppVersion :: Maybe Text
  }
  deriving (Generic, Show, HasCoordinates, FromJSON, ToJSON)

data DriverPoolWithActualDistResult = DriverPoolWithActualDistResult
  { driverPoolResult :: DriverPoolResult,
    actualDistanceToPickup :: Meters,
    actualDurationToPickup :: Seconds,
    keepHiddenForSeconds :: Seconds,
    intelligentScores :: IntelligentScores,
    isPartOfIntelligentPool :: Bool,
    pickupZone :: Bool,
    specialZoneExtraTip :: Maybe Money,
    goHomeReqId :: Maybe (Id DDGR.DriverGoHomeRequest)
  }
  deriving (Generic, Show, FromJSON, ToJSON)

instance HasCoordinates DriverPoolWithActualDistResult where
  getCoordinates r = getCoordinates r.driverPoolResult

data DriverPoolWithActualDistResultWithFlags = DriverPoolWithActualDistResultWithFlags
  { driverPoolWithActualDistResult :: [DriverPoolWithActualDistResult],
    poolType :: PoolType,
    prevBatchDrivers :: [Id Driver],
    nextScheduleTime :: Maybe Seconds
  }

castServiceTierToVariant :: DVST.ServiceTierType -> Vehicle.Variant
castServiceTierToVariant = \case
  DVST.SEDAN -> Vehicle.SEDAN
  DVST.HATCHBACK -> Vehicle.HATCHBACK
  DVST.TAXI -> Vehicle.TAXI
  DVST.SUV -> Vehicle.SUV
  DVST.TAXI_PLUS -> Vehicle.TAXI_PLUS
  DVST.AUTO_RICKSHAW -> Vehicle.AUTO_RICKSHAW
  DVST.BIKE -> Vehicle.BIKE
  _ -> Vehicle.SEDAN

castVariantToServiceTier :: Vehicle.Variant -> DVST.ServiceTierType
castVariantToServiceTier = \case
  Vehicle.SEDAN -> DVST.SEDAN
  Vehicle.HATCHBACK -> DVST.HATCHBACK
  Vehicle.TAXI -> DVST.TAXI
  Vehicle.SUV -> DVST.SUV
  Vehicle.TAXI_PLUS -> DVST.TAXI_PLUS
  Vehicle.AUTO_RICKSHAW -> DVST.AUTO_RICKSHAW
  Vehicle.BIKE -> DVST.BIKE
