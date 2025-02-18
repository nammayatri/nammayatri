{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-orphans #-}

module SharedLogic.DriverPool.Types where

import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A
import Data.Default.Class
import qualified Domain.Types as DTC
import qualified Domain.Types as DVST
import Domain.Types.Common as DI (DriverMode (..))
import qualified Domain.Types.DriverGoHomeRequest as DDGR
import Domain.Types.DriverIntelligentPoolConfig (IntelligentScores (..))
import Domain.Types.DriverPoolConfig (DriverPoolConfig)
import Domain.Types.GoHomeConfig (GoHomeConfig)
import qualified Domain.Types.Merchant as DM
import Domain.Types.Person (Driver)
import qualified Domain.Types.SearchRequest as DSR
import qualified Domain.Types.SearchTry as DST
import qualified Domain.Types.TransporterConfig as DTC
import qualified Domain.Types.VehicleVariant as Vehicle
import EulerHS.Prelude hiding (id)
import qualified Kernel.External.Maps as Maps
import qualified Kernel.External.Notification.FCM.Types as FCM
import Kernel.Types.Id
import Kernel.Types.Version
import Kernel.Utils.Common
import Lib.Scheduler.Types
import qualified SharedLogic.Beckn.Common as DTS
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
    isInterCity :: Bool,
    isValueAddNP :: Bool,
    onlinePayment :: Bool,
    currentSearchInfo :: DTS.CurrentSearchInfo,
    transporterConfig :: DTC.TransporterConfig
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
    variant :: Vehicle.VehicleVariant,
    serviceTier :: DVST.ServiceTierType,
    serviceTierDowngradeLevel :: Int,
    isAirConditioned :: Maybe Bool,
    lat :: Double,
    lon :: Double,
    mode :: Maybe DriverMode,
    vehicleAge :: Maybe Months,
    clientSdkVersion :: Maybe Version,
    clientBundleVersion :: Maybe Version,
    clientConfigVersion :: Maybe Version,
    clientDevice :: Maybe Device,
    backendConfigVersion :: Maybe Version,
    backendAppVersion :: Maybe Text,
    latestScheduledBooking :: Maybe UTCTime,
    latestScheduledPickup :: Maybe Maps.LatLong,
    customerTags :: Maybe A.Value,
    driverTags :: A.Value,
    score :: Maybe A.Value
  }
  deriving (Generic, Show, HasCoordinates, FromJSON, ToJSON)

-- Used for Tagging logic testing
instance Default DriverPoolResult where
  def =
    DriverPoolResult
      { driverId = "",
        language = Nothing,
        driverDeviceToken = Nothing,
        distanceToPickup = Meters 0,
        variant = Vehicle.AUTO_RICKSHAW,
        serviceTier = DVST.AUTO_RICKSHAW,
        serviceTierDowngradeLevel = 0,
        isAirConditioned = Nothing,
        lat = 0.0,
        lon = 0.0,
        mode = Just DI.ONLINE,
        vehicleAge = Nothing,
        clientSdkVersion = Nothing,
        clientBundleVersion = Nothing,
        clientConfigVersion = Nothing,
        clientDevice = Nothing,
        backendConfigVersion = Nothing,
        backendAppVersion = Nothing,
        latestScheduledBooking = Nothing,
        latestScheduledPickup = Nothing,
        customerTags = Nothing,
        driverTags = A.emptyObject,
        score = Nothing
      }

data DriverPoolResultCurrentlyOnRide = DriverPoolResultCurrentlyOnRide
  { driverId :: Id Driver,
    language :: Maybe Maps.Language,
    driverDeviceToken :: Maybe FCM.FCMRecipientToken,
    variant :: Vehicle.VehicleVariant,
    serviceTier :: DVST.ServiceTierType,
    serviceTierDowngradeLevel :: Int,
    isAirConditioned :: Maybe Bool,
    lat :: Double,
    lon :: Double,
    previousRideDropLat :: Double,
    previousRideDropLon :: Double,
    distanceToPickup :: Meters,
    distanceFromDriverToDestination :: Meters,
    mode :: Maybe DriverMode,
    clientSdkVersion :: Maybe Version,
    clientBundleVersion :: Maybe Version,
    vehicleAge :: Maybe Months,
    clientConfigVersion :: Maybe Version,
    clientDevice :: Maybe Device,
    backendConfigVersion :: Maybe Version,
    backendAppVersion :: Maybe Text,
    latestScheduledBooking :: Maybe UTCTime,
    latestScheduledPickup :: Maybe Maps.LatLong,
    driverTags :: A.Value,
    score :: Maybe A.Value
  }
  deriving (Generic, Show, HasCoordinates, FromJSON, ToJSON)

data DriverPoolTags = GoHomeDriverToDestination | GoHomeDriverNotToDestination | SpecialZoneQueueDriver | NormalDriver | OnRideDriver | FavouriteDriver
  deriving (Generic, Show, FromJSON, ToJSON)

data DriverPoolWithActualDistResult = DriverPoolWithActualDistResult
  { driverPoolResult :: DriverPoolResult,
    actualDistanceToPickup :: Meters,
    actualDurationToPickup :: Seconds,
    keepHiddenForSeconds :: Seconds,
    intelligentScores :: IntelligentScores,
    isPartOfIntelligentPool :: Bool,
    pickupZone :: Bool,
    specialZoneExtraTip :: Maybe HighPrecMoney,
    searchTags :: Maybe A.Value,
    tripDistance :: Maybe Meters,
    isForwardRequest :: Bool,
    previousDropGeoHash :: Maybe Text,
    goHomeReqId :: Maybe (Id DDGR.DriverGoHomeRequest),
    score :: Maybe A.Value
  }
  deriving (Generic, Show, FromJSON, ToJSON)

-- Used for Tagging logic testing
instance Default DriverPoolWithActualDistResult where
  def =
    DriverPoolWithActualDistResult
      { driverPoolResult = def,
        actualDistanceToPickup = Meters 0,
        actualDurationToPickup = Seconds 0,
        keepHiddenForSeconds = Seconds 0,
        intelligentScores = def,
        isPartOfIntelligentPool = False,
        pickupZone = False,
        specialZoneExtraTip = Nothing,
        searchTags = Nothing,
        tripDistance = Nothing,
        isForwardRequest = False,
        previousDropGeoHash = Nothing,
        goHomeReqId = Nothing,
        score = Nothing
      }

instance HasCoordinates DriverPoolWithActualDistResult where
  getCoordinates r = getCoordinates r.driverPoolResult

instance Default IntelligentScores where
  def =
    IntelligentScores
      { acceptanceRatio = Nothing,
        actualPickupDistanceScore = Nothing,
        availableTime = Nothing,
        cancellationRatio = Nothing,
        driverSpeed = Nothing,
        rideFrequency = Nothing,
        rideRequestPopupDelayDuration = 0
      }

data TaggedDriverPoolInput = TaggedDriverPoolInput
  { drivers :: [DriverPoolWithActualDistResult],
    needOnRideDrivers :: Bool,
    batchNum :: PoolBatchNum
  }
  deriving (Generic, Show, FromJSON, ToJSON)

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
    estimateOrQuoteId :: Text,
    eligibleForUpgrade :: Bool
  }

data DriverSearchBatchInput m = DriverSearchBatchInput
  { sendSearchRequestToDrivers :: DriverPoolConfig -> DST.SearchTry -> DriverSearchBatchInput m -> GoHomeConfig -> m (ExecutionResult, PoolType, Maybe Seconds),
    merchant :: DM.Merchant,
    searchReq :: DSR.SearchRequest,
    tripQuoteDetails :: [TripQuoteDetail],
    customerExtraFee :: Maybe HighPrecMoney,
    messageId :: Text,
    isRepeatSearch :: Bool,
    isAllocatorBatch :: Bool
  }
