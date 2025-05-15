module SharedLogic.Scheduler.Jobs.MetroBusinessHour where

import qualified BecknV2.FRFS.Enums as Spec
import qualified BecknV2.OnDemand.Enums as BecknSpec
import qualified Data.Text as T
import qualified Data.Time as Time
import qualified Domain.Types.IntegratedBPPConfig as DIBC
import qualified Domain.Types.RiderConfig as RC
import EulerHS.Prelude ((+||), (||+))
import qualified ExternalBPP.ExternalAPI.CallAPI as CallAPI
import Kernel.Prelude
import qualified Kernel.Storage.Hedis.Queries as Hedis
import Kernel.Utils.Common
import Lib.Scheduler
import SharedLogic.JobScheduler
import qualified Storage.CachedQueries.IntegratedBPPConfig as CQIntBPP
import qualified Storage.Queries.RiderConfig as QRC
import Tools.Error

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
  let (year, month, day) = Time.toGregorian (Time.utctDay now)
      tomorrow = Time.fromGregorian year month (day + 1)
      tomorrowAt5AM = Time.UTCTime tomorrow (5 * 3600) -- 5:00 AM
  integBPPConfig <-
    CQIntBPP.findByDomainAndCityAndVehicleCategory (show Spec.FRFS) merchantOpCityId (BecknSpec.METRO) DIBC.MULTIMODAL
      >>= fromMaybeM (IntegratedBPPConfigNotFound $ "MerchantOperatingCityId:" +|| merchantOpCityId.getId ||+ "Domain:" +|| Spec.FRFS ||+ "Vehicle:" +|| BecknSpec.METRO ||+ "Platform Type:" +|| DIBC.MULTIMODAL ||+ "")

  if isCMRLConfig integBPPConfig
    then do
      -- Get business hours from CMRL API
      businessHourResult <- CallAPI.getBusinessHour integBPPConfig
      logInfo $ "Fetched business hours from CMRL API: " <> show businessHourResult

      mbRiderConfig <- QRC.findByMerchantOperatingCityId merchantOpCityId
      case mbRiderConfig of
        Nothing -> do
          logWarning $ "RiderConfig not found for merchantOpCityId: " <> merchantOpCityId.getId
          handleJobRetry retryKeyPrefix merchantOpCityId.getId maxRetries retryInterval tomorrowAt5AM
        Just riderConfig -> do
          let startRestrictionTime = businessHourResult.qrTicketRestrictionStartTime
              endRestrictionTime = businessHourResult.qrTicketRestrictionEndTime

          let parseTimeOfDay = Time.parseTimeM True Time.defaultTimeLocale "%H:%M:%S" . T.unpack
              mbStartTime = parseTimeOfDay startRestrictionTime
              mbEndTime = parseTimeOfDay endRestrictionTime

          case (mbStartTime, mbEndTime) of
            (Just startTime, Just endTime) -> do
              let updatedConfig =
                    riderConfig
                      { RC.qrTicketRestrictionEndTime = Just endTime,
                        RC.qrTicketRestrictionStartTime = Just startTime
                      }
              QRC.updateByPrimaryKey updatedConfig

              -- Reset retry counter on success
              resetRetryCounter retryKeyPrefix merchantOpCityId.getId

              return $ ReSchedule tomorrowAt5AM
            _ -> do
              logError $ "Failed to parse restriction times: start=" <> startRestrictionTime <> ", end=" <> endRestrictionTime
              handleJobRetry retryKeyPrefix merchantOpCityId.getId maxRetries retryInterval tomorrowAt5AM
    else do
      logWarning $ "No CMRL integration found for merchant operating city: " <> merchantOpCityId.getId
      handleJobRetry retryKeyPrefix merchantOpCityId.getId maxRetries retryInterval tomorrowAt5AM
  where
    isCMRLConfig config = case config.providerConfig of
      DIBC.CMRL _ -> True
      _ -> False

handleJobRetry ::
  (MonadFlow m, CacheFlow m r) =>
  Text ->
  Text ->
  Int ->
  NominalDiffTime ->
  UTCTime ->
  m ExecutionResult
handleJobRetry keyPrefix merchantOpCityId maxRetries retryInterval nextScheduledTime = do
  let retryKey = keyPrefix <> ":" <> merchantOpCityId
  now <- getCurrentTime

  -- Get current count, set to 0 if not exists
  currentCount <-
    Hedis.get retryKey >>= \case
      Just count -> return count
      Nothing -> do
        -- Key doesn't exist, initialize it
        void $ Hedis.set retryKey (0 :: Int)
        -- Set expiry to end of day
        let (year, month, day) = Time.toGregorian (Time.utctDay now)
            nextDay = Time.fromGregorian year month (day + 1)
            midnight = Time.UTCTime nextDay 0
            secsUntilMidnight = round $ diffUTCTime midnight now
        Hedis.expire retryKey secsUntilMidnight
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
