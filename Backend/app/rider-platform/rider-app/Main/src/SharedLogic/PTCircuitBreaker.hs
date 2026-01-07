{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.PTCircuitBreaker
  ( PTMode (..),
    APIType (..),
    ThresholdConfig (..),
    CircuitBreakerConfig (..),
    recordFailure,
    recordSuccess,
    isCircuitOpen,
    tryAcquireCanarySlot,
    checkAndDisableIfNeeded,
    reEnableCircuit,
    parseCircuitBreakerConfig,
    vehicleCategoryToPTMode,
  )
where

import qualified BecknV2.FRFS.Enums as Spec
import Data.Aeson
import Data.Aeson.Types (parseMaybe)
import qualified Data.Text as T
import Domain.Types.MerchantOperatingCity (MerchantOperatingCity)
import qualified Domain.Types.RiderConfig as DRC
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Queries.RiderConfig as QRiderConfig

-- | Public transport mode
data PTMode = Metro | Bus | Subway
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- | API type for circuit breaker tracking
data APIType = FareAPI | BookingAPI
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- | Threshold configuration for triggering circuit breaker
data ThresholdConfig = ThresholdConfig
  { failureCount :: Int,
    windowSeconds :: Int
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- | Circuit breaker configuration for a specific API type
data APICircuitBreakerConfig = APICircuitBreakerConfig
  { thresholds :: [ThresholdConfig],
    canaryAllowedPerWindow :: Int,
    canaryWindowSeconds :: Int
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- | Full circuit breaker configuration
data CircuitBreakerConfig = CircuitBreakerConfig
  { fare :: Maybe APICircuitBreakerConfig,
    booking :: Maybe APICircuitBreakerConfig
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

-- | Default configuration if none is provided
defaultCircuitBreakerConfig :: CircuitBreakerConfig
defaultCircuitBreakerConfig =
  CircuitBreakerConfig
    { fare =
        Just
          APICircuitBreakerConfig
            { thresholds = [ThresholdConfig 5 60, ThresholdConfig 10 300],
              canaryAllowedPerWindow = 2,
              canaryWindowSeconds = 60
            },
      booking =
        Just
          APICircuitBreakerConfig
            { thresholds = [ThresholdConfig 3 60, ThresholdConfig 5 300],
              canaryAllowedPerWindow = 1,
              canaryWindowSeconds = 120
            }
    }

-- | Parse circuit breaker config from JSON Value
parseCircuitBreakerConfig :: Maybe Value -> CircuitBreakerConfig
parseCircuitBreakerConfig Nothing = defaultCircuitBreakerConfig
parseCircuitBreakerConfig (Just val) =
  fromMaybe defaultCircuitBreakerConfig $ parseMaybe parseJSON val

-- | Convert vehicle category to PT mode
vehicleCategoryToPTMode :: Spec.VehicleCategory -> PTMode
vehicleCategoryToPTMode Spec.METRO = Metro
vehicleCategoryToPTMode Spec.BUS = Bus
vehicleCategoryToPTMode Spec.SUBWAY = Subway

-- | Redis key for failure tracking (sorted set)
mkFailureKey :: Id MerchantOperatingCity -> PTMode -> APIType -> Text
mkFailureKey mocId mode apiType =
  "pt:failures:" <> mocId.getId <> ":" <> T.pack (show mode) <> ":" <> T.pack (show apiType)

-- | Redis key for canary counter
mkCanaryKey :: Id MerchantOperatingCity -> PTMode -> APIType -> Text
mkCanaryKey mocId mode apiType =
  "pt:canary:" <> mocId.getId <> ":" <> T.pack (show mode) <> ":" <> T.pack (show apiType)

-- | Record a failure in the sliding window
recordFailure ::
  (MonadFlow m, Hedis.HedisFlow m r) =>
  PTMode ->
  APIType ->
  Id MerchantOperatingCity ->
  m ()
recordFailure mode apiType mocId = do
  now <- getCurrentTime
  let key = mkFailureKey mocId mode apiType
      score = round (utcToMilliseconds now) :: Integer
      member = show now -- Use timestamp as member for uniqueness
  void $ Hedis.zAddExp key member score 600 -- 10 min TTL for cleanup

-- | Record a success (clears recent failures to prevent false positives)
recordSuccess ::
  (MonadFlow m, Hedis.HedisFlow m r) =>
  PTMode ->
  APIType ->
  Id MerchantOperatingCity ->
  m ()
recordSuccess mode apiType mocId = do
  let key = mkFailureKey mocId mode apiType
  -- On success, we can optionally remove some failures or just let them expire
  -- For now, we'll remove all failures to reset the circuit
  void $ Hedis.del key

-- | Check if circuit is open based on RiderConfig flags
isCircuitOpen ::
  PTMode ->
  APIType ->
  Maybe DRC.RiderConfig ->
  Bool
isCircuitOpen mode FareAPI mRiderConfig =
  case mode of
    Metro -> not $ fromMaybe True (mRiderConfig >>= (.metroFareCachingAllowed))
    Bus -> not $ fromMaybe True (mRiderConfig >>= (.busFareCachingAllowed))
    Subway -> not $ fromMaybe True (mRiderConfig >>= (.suburbanFareCachingAllowed))
isCircuitOpen mode BookingAPI mRiderConfig =
  case mode of
    Metro -> not $ fromMaybe True (mRiderConfig >>= (.metroBookingAllowed))
    Bus -> not $ fromMaybe True (mRiderConfig >>= (.busBookingAllowed))
    Subway -> not $ fromMaybe True (mRiderConfig >>= (.suburbanBookingAllowed))

-- | Try to acquire a canary slot (returns True if allowed to proceed)
tryAcquireCanarySlot ::
  (MonadFlow m, Hedis.HedisFlow m r) =>
  PTMode ->
  APIType ->
  Id MerchantOperatingCity ->
  Int -> -- Max canary requests allowed
  Int -> -- Window in seconds
  m Bool
tryAcquireCanarySlot mode apiType mocId maxAllowed windowSeconds = do
  let key = mkCanaryKey mocId mode apiType
  count <- Hedis.incr key
  when (count == 1) $ void $ Hedis.expire key windowSeconds
  return $ count <= fromIntegral maxAllowed

-- | Check if threshold is exceeded and disable if needed
checkAndDisableIfNeeded ::
  (MonadFlow m, Hedis.HedisFlow m r, EsqDBFlow m r, CacheFlow m r) =>
  PTMode ->
  APIType ->
  Id MerchantOperatingCity ->
  CircuitBreakerConfig ->
  m ()
checkAndDisableIfNeeded mode apiType mocId config = do
  let apiConfig = case apiType of
        FareAPI -> config.fare
        BookingAPI -> config.booking
  case apiConfig of
    Nothing -> return ()
    Just cfg -> do
      shouldDisable <- checkThresholds mocId mode apiType cfg.thresholds
      when shouldDisable $ disableCircuit mode apiType mocId

-- | Check if any threshold is exceeded
checkThresholds ::
  (MonadFlow m, Hedis.HedisFlow m r) =>
  Id MerchantOperatingCity ->
  PTMode ->
  APIType ->
  [ThresholdConfig] ->
  m Bool
checkThresholds mocId mode apiType thresholds = do
  now <- getCurrentTime
  let key = mkFailureKey mocId mode apiType
  results <- forM thresholds $ \threshold -> do
    let windowStart = addUTCTime (fromIntegral (negate threshold.windowSeconds)) now
        minScore = utcToMilliseconds windowStart
        maxScore = utcToMilliseconds now
    count <- Hedis.zCount key minScore maxScore
    return $ count >= fromIntegral threshold.failureCount
  return $ or results

-- | Disable the circuit by updating RiderConfig
disableCircuit ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  PTMode ->
  APIType ->
  Id MerchantOperatingCity ->
  m ()
disableCircuit mode apiType mocId = do
  logWarning $ "PT Circuit Breaker: Disabling " <> T.pack (show mode) <> " " <> T.pack (show apiType) <> " for city " <> mocId.getId
  case apiType of
    FareAPI -> updateFareCachingFlag mode mocId False
    BookingAPI -> updateBookingFlag mode mocId False

-- | Re-enable the circuit after successful canary request
reEnableCircuit ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r, Hedis.HedisFlow m r) =>
  PTMode ->
  APIType ->
  Id MerchantOperatingCity ->
  m ()
reEnableCircuit mode apiType mocId = do
  logInfo $ "PT Circuit Breaker: Re-enabling " <> T.pack (show mode) <> " " <> T.pack (show apiType) <> " for city " <> mocId.getId <> " after successful canary"
  -- Clear failure history
  let failureKey = mkFailureKey mocId mode apiType
  void $ Hedis.del failureKey
  -- Re-enable the flag
  case apiType of
    FareAPI -> updateFareCachingFlag mode mocId True
    BookingAPI -> updateBookingFlag mode mocId True

-- | Update fare caching flag in RiderConfig
updateFareCachingFlag ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  PTMode ->
  Id MerchantOperatingCity ->
  Bool ->
  m ()
updateFareCachingFlag mode mocId enabled = do
  mRiderConfig <- QRiderConfig.findByMerchantOperatingCityId mocId
  case mRiderConfig of
    Nothing -> logError $ "RiderConfig not found for mocId: " <> mocId.getId
    Just riderConfig -> do
      let updatedConfig = case mode of
            Metro -> riderConfig {DRC.metroFareCachingAllowed = Just enabled}
            Bus -> riderConfig {DRC.busFareCachingAllowed = Just enabled}
            Subway -> riderConfig {DRC.suburbanFareCachingAllowed = Just enabled}
      QRiderConfig.updateByPrimaryKey updatedConfig

-- | Update booking flag in RiderConfig
updateBookingFlag ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r) =>
  PTMode ->
  Id MerchantOperatingCity ->
  Bool ->
  m ()
updateBookingFlag mode mocId enabled = do
  mRiderConfig <- QRiderConfig.findByMerchantOperatingCityId mocId
  case mRiderConfig of
    Nothing -> logError $ "RiderConfig not found for mocId: " <> mocId.getId
    Just riderConfig -> do
      let updatedConfig = case mode of
            Metro -> riderConfig {DRC.metroBookingAllowed = Just enabled}
            Bus -> riderConfig {DRC.busBookingAllowed = Just enabled}
            Subway -> riderConfig {DRC.suburbanBookingAllowed = Just enabled}
      QRiderConfig.updateByPrimaryKey updatedConfig
