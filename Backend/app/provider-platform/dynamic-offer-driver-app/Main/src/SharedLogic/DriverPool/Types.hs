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
import qualified Data.Aeson.KeyMap as AKM
import qualified Data.Aeson.Types as A
import Data.Default.Class
import qualified Domain.Types as DTC
import qualified Domain.Types as DVST
import Domain.Types.Common as DI (DriverMode (..))
import qualified Domain.Types.ConditionalCharges as DAC
import qualified Domain.Types.DriverGoHomeRequest as DDGR
import Domain.Types.DriverIntelligentPoolConfig (IntelligentScores (..))
import Domain.Types.DriverPoolConfig (DriverPoolConfig)
import qualified Domain.Types.Extra.MerchantPaymentMethod as DMPM
import Domain.Types.GoHomeConfig (GoHomeConfig)
import qualified Domain.Types.Merchant as DM
import Domain.Types.Person (Driver)
import qualified Domain.Types.Person as Person
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
import qualified Lib.Types.SpecialLocation as SL
import qualified Lib.Yudhishthira.Types as LYT
import qualified SharedLogic.Beckn.Common as DTS
import qualified SharedLogic.Type as SLT
import Tools.Maps as Google

type PoolBatchNum = Int

type PoolRadiusStep = Meters

data PoolCalculationStage = Estimate | DriverSelection deriving (Eq, Show)

data PoolType = NormalPool | GoHomePool | SpecialDriversPool | SpecialZoneQueuePool | SkipPool deriving (Ord, Eq, Show)

data NearestGoHomeDriversResult = NearestGoHomeDriversResult
  { driverId :: Id Driver,
    driverDeviceToken :: Maybe FCM.FCMRecipientToken,
    language :: Maybe Maps.Language,
    onRide :: Bool,
    distanceToDriver :: Meters,
    driverGender :: Maybe Person.Gender,
    variant :: Vehicle.VehicleVariant,
    serviceTier :: DVST.ServiceTierType,
    serviceTierDowngradeLevel :: Int,
    isAirConditioned :: Maybe Bool,
    isSpecialLocWarrior :: Bool,
    lat :: Double,
    lon :: Double,
    mode :: Maybe DI.DriverMode,
    clientSdkVersion :: Maybe Version,
    clientBundleVersion :: Maybe Version,
    reactBundleVersion :: Maybe Text,
    clientConfigVersion :: Maybe Version,
    clientDevice :: Maybe Device,
    vehicleAge :: Maybe Months,
    backendConfigVersion :: Maybe Version,
    backendAppVersion :: Maybe Text,
    latestScheduledBooking :: Maybe UTCTime,
    latestScheduledPickup :: Maybe Maps.LatLong,
    driverTags :: A.Value,
    score :: Maybe A.Value,
    tripDistanceMinThreshold :: Maybe Meters,
    tripDistanceMaxThreshold :: Maybe Meters,
    isTollRouteEligible :: Bool,
    fleetOwnerId :: Maybe Text
  }
  deriving (Generic, Show, HasCoordinates)

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
    rideFare :: Maybe HighPrecMoney,
    govtCharges :: Maybe HighPrecMoney,
    tollCharges :: Maybe HighPrecMoney,
    parkingCharge :: Maybe HighPrecMoney,
    paymentInstrument :: Maybe DMPM.PaymentInstrument,
    currentSearchInfo :: DTS.CurrentSearchInfo,
    transporterConfig :: DTC.TransporterConfig,
    configsInExperimentVersions :: [LYT.ConfigVersionMap],
    paymentMode :: Maybe DMPM.PaymentMode
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
    reactBundleVersion :: Maybe Text,
    clientConfigVersion :: Maybe Version,
    clientDevice :: Maybe Device,
    backendConfigVersion :: Maybe Version,
    backendAppVersion :: Maybe Text,
    latestScheduledBooking :: Maybe UTCTime,
    latestScheduledPickup :: Maybe Maps.LatLong,
    customerTags :: Maybe A.Value,
    driverTags :: A.Value,
    score :: Maybe A.Value,
    minRideDistance :: Maybe Meters,
    maxRideDistance :: Maybe Meters,
    maxPickupDistance :: Maybe Meters,
    isPetModeEnabled :: Bool,
    isTollRouteEligible :: Bool, -- True if driver is not blocked for toll routes
    driverGender :: Maybe Person.Gender,
    vehicleNumber :: Maybe Text,
    fleetOwnerId :: Maybe Text,
    -- On-ride forward batching fields (Nothing for non-on-ride drivers)
    onRide :: Maybe Bool,
    previousRideDropLat :: Maybe Double,
    previousRideDropLon :: Maybe Double,
    distanceFromDriverToDestination :: Maybe Meters
  }
  deriving (Generic, Show, HasCoordinates, ToJSON)

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
        reactBundleVersion = Nothing,
        clientConfigVersion = Nothing,
        clientDevice = Nothing,
        backendConfigVersion = Nothing,
        backendAppVersion = Nothing,
        latestScheduledBooking = Nothing,
        latestScheduledPickup = Nothing,
        customerTags = Nothing,
        driverTags = A.emptyObject,
        score = Nothing,
        minRideDistance = Nothing,
        maxRideDistance = Nothing,
        maxPickupDistance = Nothing,
        isPetModeEnabled = False,
        isTollRouteEligible = True,
        driverGender = Nothing,
        vehicleNumber = Nothing,
        fleetOwnerId = Nothing,
        onRide = Nothing,
        previousRideDropLat = Nothing,
        previousRideDropLon = Nothing,
        distanceFromDriverToDestination = Nothing
      }

data DriverPoolResultCurrentlyOnRide = DriverPoolResultCurrentlyOnRide
  { driverId :: Id Driver,
    language :: Maybe Maps.Language,
    driverDeviceToken :: Maybe FCM.FCMRecipientToken,
    variant :: Vehicle.VehicleVariant,
    serviceTier :: DVST.ServiceTierType,
    serviceTierDowngradeLevel :: Int,
    isAirConditioned :: Maybe Bool,
    driverGender :: Maybe Person.Gender,
    lat :: Double,
    lon :: Double,
    previousRideDropLat :: Double,
    previousRideDropLon :: Double,
    distanceToPickup :: Meters,
    distanceFromDriverToDestination :: Meters,
    mode :: Maybe DriverMode,
    clientSdkVersion :: Maybe Version,
    clientBundleVersion :: Maybe Version,
    reactBundleVersion :: Maybe Text,
    vehicleAge :: Maybe Months,
    clientConfigVersion :: Maybe Version,
    clientDevice :: Maybe Device,
    backendConfigVersion :: Maybe Version,
    backendAppVersion :: Maybe Text,
    latestScheduledBooking :: Maybe UTCTime,
    latestScheduledPickup :: Maybe Maps.LatLong,
    driverTags :: A.Value,
    score :: Maybe A.Value,
    minRideDistance :: Maybe Meters,
    maxRideDistance :: Maybe Meters,
    maxPickupDistance :: Maybe Meters,
    isPetModeEnabled :: Bool,
    isTollRouteEligible :: Bool, -- True if driver is not blocked for toll routes
    vehicleNumber :: Maybe Text,
    fleetOwnerId :: Maybe Text
  }
  deriving (Generic, Show, HasCoordinates, FromJSON, ToJSON)

data DriverPoolTags = GoHomeDriverToDestination | GoHomeDriverNotToDestination | SpecialZoneQueueDriver | NormalDriver | OnRideDriver | FavouriteDriver | SafetyPlusDriver
  deriving (Generic, Show, FromJSON, ToJSON)

-- Per-driver sliding-window counters exposed to the POOLING dynamic-logic data.
-- Values are computed from dedicated Redis sliding-window counters (see SharedLogic.DriverPool).
data SearchReqDriverStatsCounters = SearchReqDriverStatsCounters
  { acceptanceCountToday :: Int,
    acceptanceCountWeekly :: Int,
    rejectionCountToday :: Int,
    rejectionCountWeekly :: Int,
    totalRequestsSentToday :: Int,
    totalRequestsSentWeekly :: Int,
    cancelledRidesToday :: Int,
    cancelledRidesWeekly :: Int
  }
  deriving (Generic, Show, FromJSON, ToJSON)

instance Default SearchReqDriverStatsCounters where
  def =
    SearchReqDriverStatsCounters
      { acceptanceCountToday = 0,
        acceptanceCountWeekly = 0,
        rejectionCountToday = 0,
        rejectionCountWeekly = 0,
        totalRequestsSentToday = 0,
        totalRequestsSentWeekly = 0,
        cancelledRidesToday = 0,
        cancelledRidesWeekly = 0
      }

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
    specialLocWarriorPreferredSpecialLocId :: Maybe (Id SL.SpecialLocation),
    score :: Maybe A.Value,
    -- The POOLING logic version that produced `score`. Stamped by makeTaggedDriverPool from the
    -- version it actually ran, never read back from the SearchRequest -- the two used to be
    -- sourced independently and could disagree, which silently mislabelled experiment arms.
    poolingLogicVersion :: Maybe Int,
    searchReqDriverStatsCounters :: Maybe SearchReqDriverStatsCounters,
    idleTimeSeconds :: Maybe Double,
    -- Fraction (0 to 1) of the driver's self-selected preferences (ride-distance range,
    -- pickup radius, pet mode, ...) that this specific search satisfies. See
    -- `preferenceMatchScore` below -- adding a new preference dimension never requires
    -- touching this field or its callers, only appending one more PreferenceCheck.
    preferenceMatchScore :: Double
  }
  deriving (Generic, Show, ToJSON)

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
        specialLocWarriorPreferredSpecialLocId = Nothing,
        score = Nothing,
        poolingLogicVersion = Nothing,
        searchReqDriverStatsCounters = Nothing,
        idleTimeSeconds = Nothing,
        preferenceMatchScore = 1.0
      }

-- | One preference dimension's outcome for a single driver/ride pairing.
-- `isApplicable = False` means this dimension has nothing to say about this ride
-- (driver didn't set the preference, or the preference doesn't pertain to this ride)
-- and is excluded from the score rather than counted against the driver.
--
-- `satisfaction` is a fraction in [0, 1] rather than a Bool so a dimension can
-- express a partial match (e.g. the area preference scores a pickup-only match
-- lower than a pickup-and-drop match). Binary dimensions use 'binaryCheck'.
data PreferenceCheck = PreferenceCheck
  { isApplicable :: Bool,
    satisfaction :: Double
  }

-- | A dimension that has nothing to say about this ride, and so is excluded from
-- the aggregate rather than counted as a miss.
notApplicable :: PreferenceCheck
notApplicable = PreferenceCheck {isApplicable = False, satisfaction = 0.0}

-- | Build a PreferenceCheck for a dimension that is either fully met or not met
-- at all. Keeps binary call sites readable and clamps them to the same [0, 1]
-- scale the graded dimensions use.
binaryCheck :: Bool -> Bool -> PreferenceCheck
binaryCheck applicable satisfied =
  PreferenceCheck
    { isApplicable = applicable,
      satisfaction = if satisfied then 1.0 else 0.0
    }

-- | Mean satisfaction across the applicable preferences, in [0, 1].
-- No applicable preferences => 1.0 (nothing to violate => neutral/full match).
-- Each dimension's satisfaction is clamped so a malformed contributor cannot
-- drag the aggregate outside [0, 1].
computePreferenceMatchScore :: [PreferenceCheck] -> Double
computePreferenceMatchScore checks =
  case filter isApplicable checks of
    [] -> 1.0
    applicable -> sum (map (clamp01 . satisfaction) applicable) / fromIntegral (length applicable)
  where
    clamp01 = max 0.0 . min 1.0

withJsonDefault :: A.Key -> A.Value -> A.Value -> A.Value
withJsonDefault k fallback (A.Object o) | not (AKM.member k o) = A.Object (AKM.insert k fallback o)
withJsonDefault _ _ v = v

instance FromJSON DriverPoolResult where
  -- absent isPetModeEnabled => driver had not opted into pet rides
  parseJSON = A.genericParseJSON A.defaultOptions . withJsonDefault "isPetModeEnabled" (A.Bool False)

instance FromJSON DriverPoolWithActualDistResult where
  parseJSON = A.genericParseJSON A.defaultOptions . withJsonDefault "preferenceMatchScore" (A.Number 1.0)

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
    batchNum :: PoolBatchNum,
    -- | Rejects so far in this search try, across all its batches. Lets a POOLING rule react to
    -- a pool that keeps refusing instead of only seeing the batch in front of it.
    --
    -- Maybe on purpose: this type is also how the ruleset's *output* is parsed, and a live
    -- ruleset that rebuilds the object without this key would otherwise fail to decode and
    -- silently drop the whole ranking back to the unsorted pool.
    cumulativeRejectCount :: Maybe Int
  }
  deriving (Generic, Show, FromJSON, ToJSON)

instance Default TaggedDriverPoolInput where
  def =
    TaggedDriverPoolInput
      { drivers = [],
        needOnRideDrivers = False,
        batchNum = 0,
        cumulativeRejectCount = Just 0
      }

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
    tollCharges :: Maybe HighPrecMoney,
    driverMinFee :: Maybe HighPrecMoney,
    driverMaxFee :: Maybe HighPrecMoney,
    driverStepFee :: Maybe HighPrecMoney,
    driverDefaultStepFee :: Maybe HighPrecMoney,
    driverPickUpCharge :: Maybe HighPrecMoney,
    driverParkingCharge :: Maybe HighPrecMoney,
    conditionalCharges :: [DAC.ConditionalCharges],
    congestionCharges :: Maybe HighPrecMoney,
    petCharges :: Maybe HighPrecMoney,
    priorityCharges :: Maybe HighPrecMoney,
    govtCharges :: Maybe HighPrecMoney,
    estimateOrQuoteId :: Text,
    eligibleForUpgrade :: Bool,
    commissionCharges :: Maybe HighPrecMoney,
    driverCancellationNotAllowed :: Maybe Bool
  }

data DriverSearchBatchInput m = DriverSearchBatchInput
  { sendSearchRequestToDrivers :: DriverPoolConfig -> DST.SearchTry -> DriverSearchBatchInput m -> GoHomeConfig -> m (ExecutionResult, PoolType, Maybe Seconds),
    merchant :: DM.Merchant,
    searchReq :: DSR.SearchRequest,
    tripQuoteDetails :: [TripQuoteDetail],
    customerExtraFee :: Maybe HighPrecMoney,
    messageId :: Text,
    isRepeatSearch :: Bool,
    isAllocatorBatch :: Bool,
    paymentMethodInfo :: Maybe DMPM.PaymentMethodInfo,
    billingCategory :: SLT.BillingCategory,
    emailDomain :: Maybe Text,
    businessEmailDomain :: Maybe Text,
    driverPreference :: Maybe [Text]
  }
