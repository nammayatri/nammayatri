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
    CancellationScoreRelatedConfig (..),
    DriverPoolResult (..),
    DriverPoolResultCurrentlyOnRide (..),
    DriverPoolWithActualDistResult (..),
    DriverPoolWithActualDistResultWithFlags (..),
    DriverSearchBatchInput (..),
    TripQuoteDetail (..),
    PoolType (..),
    PoolRadiusStep,
    PoolBatchNum,
    castServiceTierToVariant,
    castVariantToServiceTier,
  )
where

import qualified Domain.Types.Common as DTC
import qualified Domain.Types.DriverGoHomeRequest as DDGR
import qualified Domain.Types.DriverInformation as DI
import Domain.Types.DriverIntelligentPoolConfig (IntelligentScores)
import Domain.Types.DriverPoolConfig (DriverPoolConfig)
import Domain.Types.GoHomeConfig (GoHomeConfig)
import qualified Domain.Types.Merchant as DM
import Domain.Types.Person (Driver)
import qualified Domain.Types.SearchRequest as DSR
import qualified Domain.Types.SearchTry as DST
import qualified Domain.Types.ServiceTierType as DVST
import qualified Domain.Types.Vehicle as Vehicle
import EulerHS.Prelude hiding (id)
import qualified Kernel.External.Maps as Maps
import qualified Kernel.External.Notification.FCM.Types as FCM
import Kernel.Types.Id
import Kernel.Types.Version
import Kernel.Utils.Common
import Lib.Scheduler.Types
import Tools.Maps as Google

type PoolBatchNum = Int

type PoolRadiusStep = Meters

data PoolCalculationStage = Estimate | DriverSelection

data PoolType = NormalPool | GoHomePool | SpecialDriversPool | SpecialZoneQueuePool | SkipPool deriving (Ord, Eq, Show)

data CalculateGoHomeDriverPoolReq a = CalculateGoHomeDriverPoolReq
  { poolStage :: PoolCalculationStage,
    driverPoolCfg :: DriverPoolConfig,
    goHomeCfg :: GoHomeConfig,
    serviceTiers :: [DVST.ServiceTierType],
    fromLocation :: a,
    toLocation :: a,
    merchantId :: Id DM.Merchant,
    isRental :: Bool,
    isInterCity :: Bool
  }

data CancellationScoreRelatedConfig = CancellationScoreRelatedConfig
  { popupDelayToAddAsPenalty :: Maybe Seconds,
    thresholdCancellationScore :: Maybe Int,
    minRidesForCancellationScore :: Maybe Int
  }
  deriving (Generic)

data DriverPoolResult = DriverPoolResult
  { driverId :: Id Driver,
    language :: Maybe Maps.Language,
    driverDeviceToken :: Maybe FCM.FCMRecipientToken,
    distanceToPickup :: Meters,
    -- durationToPickup :: Seconds,
    variant :: Vehicle.Variant,
    serviceTier :: DVST.ServiceTierType,
    serviceTierDowngradeLevel :: Int,
    isAirConditioned :: Maybe Bool,
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
    serviceTierDowngradeLevel :: Int,
    isAirConditioned :: Maybe Bool,
    lat :: Double,
    lon :: Double,
    previousRideDropLat :: Double,
    previousRideDropLon :: Double,
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
    specialZoneExtraTip :: Maybe HighPrecMoney,
    isForwardRequest :: Bool,
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

data TripQuoteDetail = TripQuoteDetail
  { tripCategory :: DTC.TripCategory,
    vehicleServiceTier :: DVST.ServiceTierType,
    vehicleServiceTierName :: Text,
    baseFare :: HighPrecMoney,
    driverMinFee :: Maybe HighPrecMoney,
    driverMaxFee :: Maybe HighPrecMoney,
    driverStepFee :: Maybe HighPrecMoney,
    driverDefaultStepFee :: Maybe HighPrecMoney,
    driverPickUpCharge :: Maybe HighPrecMoney,
    driverParkingCharge :: Maybe HighPrecMoney,
    estimateOrQuoteId :: Text
  }

data DriverSearchBatchInput m = DriverSearchBatchInput
  { sendSearchRequestToDrivers :: DriverPoolConfig -> DST.SearchTry -> DriverSearchBatchInput m -> GoHomeConfig -> m (ExecutionResult, PoolType, Maybe Seconds),
    merchant :: DM.Merchant,
    searchReq :: DSR.SearchRequest,
    tripQuoteDetails :: [TripQuoteDetail],
    customerExtraFee :: Maybe HighPrecMoney,
    messageId :: Text,
    isRepeatSearch :: Bool,
    customerPhoneNum :: Maybe Text
  }

castServiceTierToVariant :: DVST.ServiceTierType -> Vehicle.Variant
castServiceTierToVariant = \case
  DVST.SEDAN -> Vehicle.SEDAN
  DVST.HATCHBACK -> Vehicle.HATCHBACK
  DVST.TAXI -> Vehicle.TAXI
  DVST.SUV -> Vehicle.SUV
  DVST.TAXI_PLUS -> Vehicle.TAXI_PLUS
  DVST.AUTO_RICKSHAW -> Vehicle.AUTO_RICKSHAW
  DVST.PREMIUM_SEDAN -> Vehicle.PREMIUM_SEDAN
  DVST.BLACK -> Vehicle.BLACK
  DVST.BLACK_XL -> Vehicle.BLACK_XL
  DVST.BIKE -> Vehicle.BIKE
  DVST.AMBULANCE_TAXI -> Vehicle.AMBULANCE_TAXI
  DVST.AMBULANCE_TAXI_OXY -> Vehicle.AMBULANCE_TAXI_OXY
  DVST.AMBULANCE_AC -> Vehicle.AMBULANCE_AC
  DVST.AMBULANCE_AC_OXY -> Vehicle.AMBULANCE_AC_OXY
  DVST.AMBULANCE_VENTILATOR -> Vehicle.AMBULANCE_VENTILATOR
  _ -> Vehicle.SEDAN

castVariantToServiceTier :: Vehicle.Variant -> DVST.ServiceTierType
castVariantToServiceTier = \case
  Vehicle.SEDAN -> DVST.SEDAN
  Vehicle.HATCHBACK -> DVST.HATCHBACK
  Vehicle.TAXI -> DVST.TAXI
  Vehicle.SUV -> DVST.SUV
  Vehicle.TAXI_PLUS -> DVST.TAXI_PLUS
  Vehicle.PREMIUM_SEDAN -> DVST.PREMIUM_SEDAN
  Vehicle.BLACK -> DVST.BLACK
  Vehicle.BLACK_XL -> DVST.BLACK_XL
  Vehicle.AUTO_RICKSHAW -> DVST.AUTO_RICKSHAW
  Vehicle.BIKE -> DVST.BIKE
  Vehicle.AMBULANCE_TAXI -> DVST.AMBULANCE_TAXI
  Vehicle.AMBULANCE_TAXI_OXY -> DVST.AMBULANCE_TAXI_OXY
  Vehicle.AMBULANCE_AC -> DVST.AMBULANCE_AC
  Vehicle.AMBULANCE_AC_OXY -> DVST.AMBULANCE_AC_OXY
  Vehicle.AMBULANCE_VENTILATOR -> DVST.AMBULANCE_VENTILATOR
