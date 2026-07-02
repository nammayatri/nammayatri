module SharedLogic.DriverPool.DriverPoolData
  ( DriverPoolData (..),
    getDriverPoolDataBatch,
    setDriverPoolData,
    setDriverPoolDataByCloud,
    driverPoolDataKey,
    defaultDriverPoolData,
    mkParallelSearchRequestKey,
    driverRequestCountKey,
    getDriverRequestCountLimit,
    checkRequestCount,
  )
where

import Data.List (nubBy)
import qualified Data.Time.Calendar as Days
import Domain.Types.Common (DriverMode)
import qualified Domain.Types.DriverGoHomeRequest as DDGR
import qualified Domain.Types.DriverInformation as DI
import Domain.Types.DriverPoolConfig (DriverPoolConfig)
import qualified Domain.Types.Extra.MerchantPaymentMethod as DMPM
import Domain.Types.Merchant (Merchant)
import Domain.Types.Person (Driver, Gender (..))
import qualified Domain.Types.SearchTry as DST
import Domain.Types.ServiceTierType (ServiceTierType)
import Domain.Types.VehicleVariant (VehicleVariant (..))
import qualified Kernel.External.Maps as Maps
import qualified Kernel.External.Notification.FCM.Types as FCM
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Id
import Kernel.Types.Version (CloudType (..), Device, Version)
import Kernel.Utils.Common hiding (ActorType (UNKNOWN))
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
    -- | Dead field, kept as Maybe so existing on-disk JSON records that carry
    -- a value still deserialise. New records leave it Nothing.
    totalRides :: Maybe Int,
    variant :: VehicleVariant,
    selectedServiceTiers :: [ServiceTierType],
    -- Class 1 (preferences)
    enabled :: Bool,
    blocked :: Bool,
    subscribed :: Bool,
    canSwitchToRental :: Bool,
    canSwitchToInterCity :: Bool,
    canSwitchToIntraCity :: Bool,
    enableForAirport :: Maybe DI.AirportRestrictionType,
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
    -- | Dead field, kept as Maybe so existing on-disk JSON records that carry
    -- a value still deserialise. New records leave it Nothing.
    safetyPlusEnabled :: Maybe Bool,
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
    cloudType :: Maybe CloudType,
    -- | Monotonic schema version stamped by the cold-start builder and by
    -- migrators in 'SharedLogic.DriverPool.DriverPoolMigrations'. 'Nothing'
    -- means the entry was written before this field existed (treated as 0,
    -- so every migrator runs on first cold read).
    schemaVersion :: Maybe Int
  }
  deriving (Generic, Show, FromJSON, ToJSON)

driverPoolDataKey :: Id Driver -> Text
driverPoolDataKey driverId = "driver-pool-data:" <> driverId.getId

-- | Redis sorted set tracking a driver's currently-pending parallel search requests.
-- Entries are added by `isLessThenNParallelRequests` (atomic reservation) and read by
-- the early parallel-request filter inside `getNearestDrivers`.
mkParallelSearchRequestKey :: Id Merchant -> Id Driver -> Text
mkParallelSearchRequestKey mId dId = "driver-offer:DriverPool:Search-Req-Validity-Map-:" <> mId.getId <> dId.getId

-- | Counter key tracking how many times a specific driver has been asked about
-- a specific search-try at a specific service tier. Incremented by
-- `incrementDriverRequestCount` after each batch is sent; checked by
-- `checkRequestCount` to cap re-asks of prev-attempted drivers within one search.
driverRequestCountKey :: Id DST.SearchTry -> Id Driver -> ServiceTierType -> Text
driverRequestCountKey searchTryId driverId vehicleServiceTier =
  "Driver-Request-Count-Key:SearchTryId-" <> searchTryId.getId <> ":DriverId-" <> driverId.getId <> ":ServiceTier-" <> show vehicleServiceTier

-- | Per-search re-ask limit. For BookAny requests with a downgrade (downgradeLevel < 0)
-- we cap at 1 attempt; otherwise use the configured `driverRequestCountLimit`.
getDriverRequestCountLimit :: Int -> Bool -> DriverPoolConfig -> Int
getDriverRequestCountLimit serviceTierDowngradeLevel isBookAnyRequest driverPoolConfig =
  if serviceTierDowngradeLevel < 0 && isBookAnyRequest
    then 1
    else driverPoolConfig.driverRequestCountLimit

-- | True if this (search-try, driver, serviceTier) is still under its re-ask cap.
-- Used as a per-search rate-limit on previously-attempted drivers.
checkRequestCount ::
  Redis.HedisFlow m r =>
  Id DST.SearchTry ->
  Bool ->
  Id Driver ->
  ServiceTierType ->
  Int ->
  DriverPoolConfig ->
  m Bool
checkRequestCount searchTryId isBookAnyRequest driverId vehicleServiceTier serviceTierDowngradeLevel driverPoolConfig =
  maybe True (\count -> (count :: Int) < getDriverRequestCountLimit serviceTierDowngradeLevel isBookAnyRequest driverPoolConfig)
    <$> Redis.withCrossAppRedis (Redis.get (driverRequestCountKey searchTryId driverId vehicleServiceTier))

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
      totalRides = Just 0,
      variant = AUTO_RICKSHAW,
      selectedServiceTiers = [],
      enabled = False,
      blocked = False,
      subscribed = False,
      canSwitchToRental = False,
      canSwitchToInterCity = False,
      canSwitchToIntraCity = False,
      enableForAirport = Nothing,
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
      safetyPlusEnabled = Just False,
      fleetOwnerId = Nothing,
      driverTripEndLocation = Nothing,
      hasRideStarted = Nothing,
      airConditionScore = Nothing,
      airConditioned = Nothing,
      luggageCapacity = Nothing,
      vehicleRating = Nothing,
      registrationNo = "",
      cloudType = Nothing,
      schemaVersion = Nothing
    }

-- | Set/overwrite the full pool data for a driver in LTS Redis.
-- Always writes to the primary cloud. Use 'setDriverPoolDataByCloud' when the
-- write should be routed to the driver's own cloud.
setDriverPoolData ::
  ( Redis.HedisFlow m r,
    MonadFlow m,
    HasField "ltsHedisEnv" r Redis.HedisEnv
  ) =>
  DriverPoolData ->
  m ()
setDriverPoolData dpd = do
  Redis.withLTSRedis $ Redis.setExp (driverPoolDataKey dpd.driverId) dpd (2592000 * 12) -- 1 year

-- | Cloud-aware write: routes the SET to primary or secondary LTS Redis
-- depending on whether the driver's cloudType matches the deployment's.
-- If both are equal (or both Nothing) → primary; otherwise → secondary.
setDriverPoolDataByCloud ::
  ( Redis.HedisFlow m r,
    MonadFlow m,
    Redis.HedisLTSFlowEnv r
  ) =>
  Maybe CloudType ->
  DriverPoolData ->
  m ()
setDriverPoolDataByCloud deploymentCloudType dpd = do
  let driverCloud = dpd.cloudType
      key = driverPoolDataKey dpd.driverId
      expiry = 2592000 * 12 -- 1 year
  if deploymentCloudType == Just (fromMaybe GCP driverCloud)
    then Redis.withLTSRedis $ Redis.setExp key dpd expiry
    else Redis.withSecondaryLTSRedis $ Redis.setExp key dpd expiry
