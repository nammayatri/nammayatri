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

-- Response Types
data DailyMetricsResponse = DailyMetricsResponse
  { toDate :: Text,
    bookingSummaryList :: [BookingSummary],
    registrationSummaryList :: [RegistrationSummary]
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
getDailyMetricsWithAuth :: Maybe Text -> Maybe Text -> Maybe Text -> Text -> FlowHandler DailyMetricsResponse
getDailyMetricsWithAuth mbApiKey mbIpAddress mbMerchantOpCityId dateStr = withFlowHandlerAPI $ do
  -- Validate API key
  validateApiKey mbApiKey

  -- IP-based rate limiting
  let clientIp = extractClientIp mbIpAddress
  checkDailyMetricsRateLimitByIp clientIp

  -- Parse merchant operating city ID from header
  merchantOpCityIdText <- fromMaybeM (InvalidRequest "x-merchant-operating-city-id header required") mbMerchantOpCityId
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
  pure $
    DailyMetricsResponse
      { toDate = dateStr,
        bookingSummaryList = metricsData.bookings,
        registrationSummaryList = metricsData.registrations
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

-- Validate API key from app config
validateApiKey :: (MonadFlow m, HasFlowEnv m r '["frfsMetricsApiKey" ::: Text]) => Maybe Text -> m ()
validateApiKey mbApiKey = do
  providedKey <- fromMaybeM (InvalidRequest "API key required") mbApiKey
  validKey <- asks (.frfsMetricsApiKey)
  unless (providedKey == validKey) $
    throwError (InvalidRequest "Invalid API key")
