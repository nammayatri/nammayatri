module SharedLogic.Scheduler.Jobs.MetroBusinessHour where

import qualified BecknV2.OnDemand.Enums as BecknSpec
import qualified Data.Text as T
import qualified Data.Time as Time
import qualified Domain.Types.IntegratedBPPConfig as DIBC
import qualified Domain.Types.RiderConfig as RC
import qualified ExternalBPP.ExternalAPI.CallAPI as CallAPI
import Kernel.Prelude
import qualified Kernel.Storage.Hedis.Queries as Hedis
import Kernel.Utils.Common
import Lib.Scheduler
import qualified SharedLogic.IntegratedBPPConfig as SIBC
import SharedLogic.JobScheduler
import qualified Storage.CachedQueries.Merchant.RiderConfig as QRCR
import Storage.ConfigPilot.Config.RiderConfig (RiderDimensions (..))
import Storage.ConfigPilot.Interface.Types (getConfig)

updateMetroBusinessHour ::
  ( EsqDBFlow m r,
    CacheFlow m r,
    MonadFlow m,
    EncFlow m r
  ) =>
  Job 'MetroBusinessHour ->
  m ExecutionResult
updateMetroBusinessHour Job {id, jobInfo} = withLogTag ("JobId-" <> id.getId) do
  let jobData = jobInfo.jobData
      merchantOpCityId = jobData.merchantOperatingCityId
      retryKeyPrefix = "MetroBusinessHourRetry"
      maxRetries = 3
      retryInterval = 30 * 60 -- 30 minutes
  now <- getCurrentTime
  let tomorrow = Time.addDays 1 (Time.utctDay now)

  -- Get rider config first to access timeDiffFromUtc
  mbRiderConfig <- getConfig (RiderDimensions {merchantOperatingCityId = merchantOpCityId.getId})
  let timeDiffFromUtc = maybe (Seconds 19800) (.timeDiffFromUtc) mbRiderConfig -- Default to IST (UTC+5:30)
      tzMinutes = getSeconds timeDiffFromUtc `div` 60
      tz = Time.minutesToTimeZone tzMinutes
      -- Calculate 00:30 local time
      localTime0030 = Time.LocalTime tomorrow (Time.TimeOfDay 0 30 0)
      tomorrowAt0030Local = Time.localTimeToUTC tz localTime0030

  integratedBppConfigs <- SIBC.findAllIntegratedBPPConfig merchantOpCityId (BecknSpec.METRO) DIBC.MULTIMODAL

  case getCMRLConfig integratedBppConfigs of
    Just integratedBppConfig -> do
      -- Get business hours from CMRL API
      businessHourResult <- CallAPI.getBusinessHour integratedBppConfig
      logInfo $ "Fetched business hours from CMRL API: " <> show businessHourResult

      case mbRiderConfig of
        Nothing -> do
          logWarning $ "RiderConfig not found for merchantOpCityId: " <> merchantOpCityId.getId
          handleJobRetry retryKeyPrefix merchantOpCityId.getId maxRetries retryInterval timeDiffFromUtc tomorrowAt0030Local
        Just riderConfig -> do
          let startRestrictionTime = businessHourResult.qrTicketRestrictionStartTime
              endRestrictionTime = businessHourResult.qrTicketRestrictionEndTime

          -- Updated time parsing to handle HH:MM format
          let parseTimeOfDay timeStr = do
                let parts = T.split (== ':') timeStr
                case parts of
                  [hours, minutes] -> do
                    h <- readMaybe (T.unpack hours)
                    m <- readMaybe (T.unpack minutes)
                    if h >= 0 && h < 24 && m >= 0 && m < 60
                      then Just $ Time.TimeOfDay h m 0
                      else Nothing
                  _ -> Nothing
              mbStartTime = parseTimeOfDay startRestrictionTime
              mbEndTime = parseTimeOfDay endRestrictionTime

          case (mbStartTime, mbEndTime) of
            (Just startTime, Just endTime) -> do
              let updatedConfig =
                    riderConfig
                      { RC.qrTicketRestrictionEndTime = Just endTime,
                        RC.qrTicketRestrictionStartTime = Just startTime
                      }
              QRCR.updateByPrimaryKey updatedConfig

              -- Reset retry counter on success
              resetRetryCounter retryKeyPrefix merchantOpCityId.getId

              return $ ReSchedule tomorrowAt0030Local
            _ -> do
              logError $ "Failed to parse restriction times: start=" <> startRestrictionTime <> ", end=" <> endRestrictionTime
              handleJobRetry retryKeyPrefix merchantOpCityId.getId maxRetries retryInterval timeDiffFromUtc tomorrowAt0030Local
    Nothing -> do
      logWarning $ "No CMRL integration found for merchant operating city: " <> merchantOpCityId.getId
      handleJobRetry retryKeyPrefix merchantOpCityId.getId maxRetries retryInterval timeDiffFromUtc tomorrowAt0030Local
  where
    getCMRLConfig :: [DIBC.IntegratedBPPConfig] -> Maybe DIBC.IntegratedBPPConfig
    getCMRLConfig integratedBppConfigs = find (\config -> case config.providerConfig of DIBC.CMRL _ -> True; _ -> False) integratedBppConfigs

handleJobRetry ::
  (MonadFlow m, CacheFlow m r) =>
  Text ->
  Text ->
  Int ->
  NominalDiffTime ->
  Seconds ->
  UTCTime ->
  m ExecutionResult
handleJobRetry keyPrefix merchantOpCityId maxRetries retryInterval timeDiffFromUtc nextScheduledTime = do
  let retryKey = keyPrefix <> ":" <> merchantOpCityId
  now <- getCurrentTime

  -- Get current count, set to 0 if not exists
  currentCount <-
    Hedis.get retryKey >>= \case
      Just count -> return count
      Nothing -> do
        -- Key doesn't exist, initialize it
        void $ Hedis.set retryKey (0 :: Int)
        -- Set expiry to end of day in local timezone
        let tomorrow = Time.addDays 1 (Time.utctDay now)
            tzMinutes = getSeconds timeDiffFromUtc `div` 60
            tz = Time.minutesToTimeZone tzMinutes
            localTimeMidnight = Time.LocalTime tomorrow (Time.TimeOfDay 0 0 0)
            midnightLocal = Time.localTimeToUTC tz localTimeMidnight
            secsUntilMidnight = round $ diffUTCTime midnightLocal now
            -- Use a minimum expiry time of 1 hour if calculated TTL is non-positive
            expirySeconds = max secsUntilMidnight (60 * 60)
        Hedis.expire retryKey expirySeconds
        return 0

  -- Check if we can retry
  if currentCount < maxRetries
    then do
      void $ Hedis.incr retryKey
      return $ ReSchedule (addUTCTime retryInterval now)
    else return $ ReSchedule nextScheduledTime

resetRetryCounter ::
  (MonadFlow m, CacheFlow m r) =>
  Text ->
  Text ->
  m ()
resetRetryCounter keyPrefix merchantOpCityId = do
  let retryKey = keyPrefix <> ":" <> merchantOpCityId
  void $ Hedis.del retryKey
