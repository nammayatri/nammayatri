module SharedLogic.DriverPool.DriverPoolData
  ( DriverPoolData (..),
    getDriverPoolDataBatch,
    setDriverPoolData,
    driverPoolDataKey,
    defaultDriverPoolData,
  )
where

import Data.List (nubBy)
import qualified Data.Time.Calendar as Days
import Domain.Types.Common (DriverMode)
import qualified Domain.Types.DriverGoHomeRequest as DDGR
import qualified Domain.Types.Extra.MerchantPaymentMethod as DMPM
import Domain.Types.Person (Driver, Gender (..))
import Domain.Types.ServiceTierType (ServiceTierType)
import Domain.Types.VehicleVariant (VehicleVariant (..))
import qualified Kernel.External.Maps as Maps
import qualified Kernel.External.Notification.FCM.Types as FCM
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Id
import Kernel.Types.Version (Device, Version)
import Kernel.Utils.Common
import qualified Lib.Yudhishthira.Types as LYT

-- | The full driver pool data record stored in Redis (later LTS).
-- This is the single key that the pooling flow reads instead of
-- querying driver_information + vehicle + person + driver_bank_account.
data DriverPoolData = DriverPoolData
  { driverId :: Id Driver,
    -- Class 2 (session state)
    active :: Bool,
    mode :: Maybe DriverMode,
    onRide :: Bool,
    onRideTripCategory :: Maybe Text,
    hasAdvanceBooking :: Maybe Bool,
    latestScheduledBooking :: Maybe UTCTime,
    latestScheduledPickup :: Maybe Maps.LatLong,
    deviceToken :: Maybe FCM.FCMRecipientToken,
    goHomeStatus :: Maybe DDGR.DriverGoHomeRequestStatus,
    totalRides :: Int,
    variant :: VehicleVariant,
    selectedServiceTiers :: [ServiceTierType],
    -- Class 1 (preferences)
    enabled :: Bool,
    blocked :: Bool,
    subscribed :: Bool,
    canSwitchToRental :: Bool,
    canSwitchToInterCity :: Bool,
    canSwitchToIntraCity :: Bool,
    forwardBatchingEnabled :: Bool,
    isSpecialLocWarrior :: Bool,
    tollRouteBlockedTill :: Maybe UTCTime,
    softBlockStiers :: Maybe [ServiceTierType],
    acUsageRestrictionType :: Maybe Text,
    acRestrictionLiftCount :: Int,
    tripDistanceMinThreshold :: Maybe Meters,
    tripDistanceMaxThreshold :: Maybe Meters,
    maxPickupRadius :: Maybe Meters,
    isPetModeEnabled :: Bool,
    chargesEnabled :: Bool,
    bankAccountPaymentMode :: Maybe DMPM.PaymentMode,
    language :: Maybe Maps.Language,
    gender :: Gender,
    driverTag :: Maybe [LYT.TagNameValueExpiry],
    clientDevice :: Maybe Device,
    clientSdkVersion :: Maybe Version,
    clientBundleVersion :: Maybe Version,
    clientConfigVersion :: Maybe Version,
    vehicleTags :: Maybe [Text],
    mYManufacturing :: Maybe Days.Day,
    safetyPlusEnabled :: Bool,
    fleetOwnerId :: Maybe Text,
    -- On-ride / forward batching fields
    driverTripEndLocation :: Maybe Maps.LatLong,
    hasRideStarted :: Maybe Bool,
    -- Vehicle attributes for service tier usage restriction checks
    airConditionScore :: Maybe Double,
    airConditioned :: Maybe Bool,
    luggageCapacity :: Maybe Int,
    vehicleRating :: Maybe Double,
    registrationNo :: Text,
    -- | Monotonic schema version stamped by the cold-start builder and by
    -- migrators in 'SharedLogic.DriverPool.DriverPoolMigrations'. 'Nothing'
    -- means the entry was written before this field existed (treated as 0,
    -- so every migrator runs on first cold read).
    schemaVersion :: Maybe Int
  }
  deriving (Generic, Show, FromJSON, ToJSON)

driverPoolDataKey :: Id Driver -> Text
driverPoolDataKey driverId = "driver-pool-data:" <> driverId.getId

-- | Batch-fetch pool data for multiple drivers from Redis.
getDriverPoolDataBatch ::
  ( Redis.HedisFlow m r,
    MonadFlow m,
    Redis.HedisLTSFlowEnv r
  ) =>
  [Id Driver] ->
  m [DriverPoolData]
getDriverPoolDataBatch driverIds = do
  let keys = map driverPoolDataKey driverIds
  primaryResults <- Redis.withLTSRedis $ Redis.mGetStandalone keys
  secondaryResults <- Redis.withSecondaryLTSRedis $ Redis.mGetStandalone keys
  pure $ nubBy (\a b -> a.driverId == b.driverId) (primaryResults <> secondaryResults)

-- | Zeroed-out DriverPoolData used as the base when no LTS entry exists yet.
-- Required non-Maybe fields use the most conservative defaults.
-- 'applyUpdate' overwrites whichever fields the caller has marked 'Set'.
defaultDriverPoolData :: Id Driver -> DriverPoolData
defaultDriverPoolData dId =
  DriverPoolData
    { driverId = dId,
      active = False,
      mode = Nothing,
      onRide = False,
      onRideTripCategory = Nothing,
      hasAdvanceBooking = Nothing,
      latestScheduledBooking = Nothing,
      latestScheduledPickup = Nothing,
      deviceToken = Nothing,
      goHomeStatus = Nothing,
      totalRides = 0,
      variant = AUTO_RICKSHAW,
      selectedServiceTiers = [],
      enabled = False,
      blocked = False,
      subscribed = False,
      canSwitchToRental = False,
      canSwitchToInterCity = False,
      canSwitchToIntraCity = False,
      forwardBatchingEnabled = False,
      isSpecialLocWarrior = False,
      tollRouteBlockedTill = Nothing,
      softBlockStiers = Nothing,
      acUsageRestrictionType = Nothing,
      acRestrictionLiftCount = 0,
      tripDistanceMinThreshold = Nothing,
      tripDistanceMaxThreshold = Nothing,
      maxPickupRadius = Nothing,
      isPetModeEnabled = False,
      chargesEnabled = False,
      bankAccountPaymentMode = Nothing,
      language = Nothing,
      gender = UNKNOWN,
      driverTag = Nothing,
      clientDevice = Nothing,
      clientSdkVersion = Nothing,
      clientBundleVersion = Nothing,
      clientConfigVersion = Nothing,
      vehicleTags = Nothing,
      mYManufacturing = Nothing,
      safetyPlusEnabled = False,
      fleetOwnerId = Nothing,
      driverTripEndLocation = Nothing,
      hasRideStarted = Nothing,
      airConditionScore = Nothing,
      airConditioned = Nothing,
      luggageCapacity = Nothing,
      vehicleRating = Nothing,
      registrationNo = "",
      schemaVersion = Nothing
    }

-- | Set/overwrite the full pool data for a driver in LTS Redis.
-- via syncDriverPoolDataToLTS. No need to expire and rebuild frequently.
setDriverPoolData ::
  ( Redis.HedisFlow m r,
    MonadFlow m,
    HasField "ltsHedisEnv" r Redis.HedisEnv
  ) =>
  DriverPoolData ->
  m ()
setDriverPoolData dpd = do
  Redis.withLTSRedis $ Redis.setExp (driverPoolDataKey dpd.driverId) dpd (2592000 * 12) -- 1 year
