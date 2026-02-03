module Domain.Action.UI.GetDailyMetrics where

import Data.OpenApi (ToSchema)
import qualified Data.Text as T
import Data.Time
import qualified Domain.Types.MerchantOperatingCity as DMOC
import Environment
import EulerHS.Prelude hiding (id)
import qualified Kernel.Storage.ClickhouseV2 as CH
import Kernel.Storage.Hedis as Redis
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import Kernel.Utils.SlidingWindowLimiter
import qualified Storage.Clickhouse.FRFSTicketBooking as CHFRFS
import qualified Storage.Clickhouse.Person as CHPerson
import qualified Storage.Queries.RiderConfig as QRiderConfig

-- Response Types
type DailyMetricsResponse = [DailyMetricRow]

data DailyMetricRow = DailyMetricRow
  { date :: Text,
    bookingCount :: Int,
    totalPassengerCount :: Int,
    totalRevenue :: Double,
    vehicleType :: Text,
    newRegistrations :: Maybe Int,
    osType :: Maybe Text
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema, Show)

data BookingSummary = BookingSummary
  { vehicleType :: Text,
    bookingCount :: Int,
    totalRevenue :: Double,
    totalPassengerCount :: Int
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema, Show)

data RegistrationSummary = RegistrationSummary
  { osType :: Text,
    newRegistrations :: Int
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema, Show)

-- Redis cache types
data RedisMetricsData = RedisMetricsData
  { bookings :: [BookingSummary],
    registrations :: [RegistrationSummary]
  }
  deriving (Generic, ToJSON, FromJSON, Show)

-- Main handler with API key authentication
getDailyMetricsWithAuth :: Maybe Text -> Maybe Text -> Text -> FlowHandler DailyMetricsResponse
getDailyMetricsWithAuth mbApiKey mbIpAddress dateStr = withFlowHandlerAPI $ do
  -- Validate API key and get merchant operating city ID from mapping
  merchantOpCityIdText <- validateApiKeyAndGetCityId mbApiKey

  -- IP-based rate limiting
  let clientIp = extractClientIp mbIpAddress
  checkDailyMetricsRateLimitByIp clientIp

  let merchantOpCityId = Id merchantOpCityIdText :: Id DMOC.MerchantOperatingCity

  let redisKey = "frfs:daily_metrics:" <> merchantOpCityIdText <> ":" <> dateStr

  -- Try to get from Redis first
  mbCachedData <- Redis.withCrossAppRedis $ Redis.safeGet redisKey

  metricsData <- case mbCachedData of
    Just cachedData -> pure cachedData
    Nothing -> do
      -- Query from ClickHouse if not in cache
      freshData <- queryMetricsFromClickHouse merchantOpCityId dateStr

      -- Store in Redis with 1 hour TTL (3600 seconds)
      Redis.withCrossAppRedis $ Redis.setExp redisKey freshData 3600

      pure freshData

  -- Return the response
  let bookings = metricsData.bookings
  let registrations = metricsData.registrations
  pure $ transformToFlat dateStr bookings registrations

transformToFlat :: Text -> [BookingSummary] -> [RegistrationSummary] -> [DailyMetricRow]
transformToFlat dateStr bookings registrations =
  let paddedRegistrations = map Just registrations ++ repeat Nothing
      pairs = zip bookings paddedRegistrations
   in map (\(b, r) -> mkRow dateStr b r) pairs

mkRow :: Text -> BookingSummary -> Maybe RegistrationSummary -> DailyMetricRow
mkRow dateStr b r =
  DailyMetricRow
    { date = dateStr,
      bookingCount = b.bookingCount,
      totalPassengerCount = b.totalPassengerCount,
      totalRevenue = b.totalRevenue,
      vehicleType = b.vehicleType,
      newRegistrations = fmap (.newRegistrations) r,
      osType = fmap (.osType) r
    }

queryMetricsFromClickHouse :: (MonadFlow m, CH.HasClickhouseEnv CH.APP_SERVICE_CLICKHOUSE m) => Id DMOC.MerchantOperatingCity -> Text -> m RedisMetricsData
queryMetricsFromClickHouse merchantOpCityId dateStr = do
  -- Parse the date string
  targetDate <- case parseTimeM True defaultTimeLocale "%Y-%m-%d" (T.unpack dateStr) of
    Nothing -> throwError $ InvalidRequest $ "Invalid date format: " <> dateStr <> ". Expected YYYY-MM-DD"
    Just day -> pure day

  bookingMetrics <- CHFRFS.getBookingMetricsByDateRange merchantOpCityId targetDate

  let bookingSummaries = map transformBookingMetrics bookingMetrics

  registrationMetrics <- CHPerson.getRegistrationMetricsByDateRange merchantOpCityId targetDate

  -- Transform to response format
  let registrationSummaries = map transformRegistrationMetrics registrationMetrics

  pure $
    RedisMetricsData
      { bookings = bookingSummaries,
        registrations = registrationSummaries
      }

-- Transform ClickHouse booking metrics to API response format
transformBookingMetrics :: CHFRFS.BookingMetrics -> BookingSummary
transformBookingMetrics metrics =
  BookingSummary
    { vehicleType = metrics.metricsVehicleType,
      bookingCount = metrics.metricsUniqueBookings,
      totalRevenue = fromMaybe 0 metrics.metricsTotalPrice,
      totalPassengerCount = metrics.metricsTotalTickets
    }

-- Transform ClickHouse registration metrics to API response format
transformRegistrationMetrics :: CHPerson.RegistrationMetrics -> RegistrationSummary
transformRegistrationMetrics metrics =
  RegistrationSummary
    { osType = fromMaybe "Unknown" metrics.metricsClientOsType,
      newRegistrations = metrics.metricsUserCount
    }

-- IP-based rate limiting
checkDailyMetricsRateLimitByIp :: (MonadFlow m, CacheFlow m r, HasFlowEnv m r '["frfsMetricsRateLimitHits" ::: Int, "frfsMetricsRateLimitWindowSec" ::: Int]) => Text -> m ()
checkDailyMetricsRateLimitByIp ipAddress = do
  hitsLimit <- asks (.frfsMetricsRateLimitHits)
  limitResetTimeInSec <- asks (.frfsMetricsRateLimitWindowSec)
  let key = dailyMetricsIpHitsCountKey ipAddress
  unlessM (slidingWindowLimiter key hitsLimit limitResetTimeInSec) $ do
    throwError $ HitsLimitError limitResetTimeInSec

dailyMetricsIpHitsCountKey :: Text -> Text
dailyMetricsIpHitsCountKey ipAddress = "FRFSMetrics:ip:" <> ipAddress <> ":hitsCount"

extractClientIp :: Maybe Text -> Text
extractClientIp Nothing = "unknown"
extractClientIp (Just forwardedFor) =
  case T.splitOn "," forwardedFor of
    [] -> "unknown"
    (firstIp : _) -> T.strip firstIp -- First IP is the real client

-- Validate API key and get merchant operating city ID from RiderConfig table
validateApiKeyAndGetCityId :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Maybe Text -> m Text
validateApiKeyAndGetCityId mbApiKey = do
  providedKey <- fromMaybeM (InvalidRequest "API key required") mbApiKey
  mbRiderConfig <- QRiderConfig.findByFrfsMetricsApiKey (Just providedKey)
  case mbRiderConfig of
    Just riderConfig -> pure $ getId riderConfig.merchantOperatingCityId
    Nothing -> throwError (InvalidRequest "Invalid API key")
