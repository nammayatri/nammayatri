module SharedLogic.DriverPool.DriverPoolData
  ( DriverPoolData (..),
    getDriverPoolDataBatch,
    setDriverPoolData,
    driverPoolDataKey,
  )
where

import qualified Data.Time.Calendar as Days
import Domain.Types.Common (DriverMode)
import qualified Domain.Types.DriverGoHomeRequest as DDGR
import Domain.Types.Person (Driver, Gender)
import Domain.Types.ServiceTierType (ServiceTierType)
import Domain.Types.VehicleVariant (VehicleVariant)
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
    vehicleRating :: Maybe Double
  }
  deriving (Generic, Show, FromJSON, ToJSON)

driverPoolDataKey :: Id Driver -> Text
driverPoolDataKey driverId = "driver-pool-data:" <> driverId.getId

-- TODO: When LTS Redis is deployed separately, add withLTSRedis swap here
-- For now, using the same Redis instance. The ltsRedisCfg in Environment.hs
-- points to the same Redis in dev, and can be configured to LTS Redis in prod.

-- | Batch-fetch pool data for multiple drivers from Redis.
getDriverPoolDataBatch ::
  (Redis.HedisFlow m r, MonadFlow m) =>
  [Id Driver] ->
  m [DriverPoolData]
getDriverPoolDataBatch driverIds = do
  results <- mapM (\did -> Redis.safeGet (driverPoolDataKey did)) driverIds
  pure $ catMaybes results

-- | Set/overwrite the full pool data for a driver in LTS Redis.
-- via syncDriverPoolDataToLTS. No need to expire and rebuild frequently.
setDriverPoolData ::
  (Redis.HedisFlow m r, MonadFlow m) =>
  DriverPoolData ->
  m ()
setDriverPoolData dpd = do
  Redis.setExp (driverPoolDataKey dpd.driverId) dpd (2592000 * 12) -- 1 year
