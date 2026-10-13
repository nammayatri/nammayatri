{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- | Proximity watchdog for a stand-by better-driver search: cancels it once the
-- currently-assigned driver gets close to pickup.
--
-- Its own job rather than part of CheckDriverPickupProgress, which doesn't run without
-- pickupStallMonitoringConfig and skips its distance maths for scheduled rides. Reads
-- LTS directly each tick rather than waiting for bulkLocPickupUpdate, which only fires
-- once ~100 points have piled up.
module SharedLogic.Allocator.Jobs.CheckBetterDriverSearchProximity (checkBetterDriverSearchProximity, betterDriverSearchProximityTickIntervalSec) where

import qualified AWS.S3 as S3
import qualified Data.HashMap.Strict as HMS
import qualified Data.Map as M
import qualified Domain.Types.SearchTry as DST
import Kernel.External.Maps (HasCoordinates (getCoordinates))
import Kernel.External.Maps.Types
import Kernel.External.Types
import Kernel.Prelude
import qualified Kernel.Storage.Clickhouse.Config as CH
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Streaming.Kafka.Producer.Types (KafkaProducerTools)
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics, DeploymentVersion)
import Kernel.Types.Version (CloudType)
import Kernel.Utils.CalculateDistance (distanceBetweenInMeters)
import Kernel.Utils.Common
import Lib.ConfigPilot.Interface.Types (getOneConfig)
import qualified Lib.Finance.Core.Types as Finance
import Lib.Finance.Storage.Beam.BeamFlow (BeamFlow)
import Lib.Scheduler
import Lib.SessionizerMetrics.Types.Event
import SharedLogic.Allocator
import SharedLogic.CallBAPInternal (AppBackendBapInternal)
import qualified SharedLogic.External.LocationTrackingService.Flow as LTSF
import qualified SharedLogic.External.LocationTrackingService.Types as LT
import SharedLogic.GoogleTranslate (TranslateFlow)
import Storage.ConfigPilot.Config.TransporterConfig (TransporterConfigDimensions (..))
import qualified Storage.Queries.Booking as QBooking
import qualified Storage.Queries.SearchTry as QST
import qualified Tools.Metrics as Metrics
import TransactionLogs.Types

-- | How often to re-check while a stand-by search is active. Not city-configurable.
betterDriverSearchProximityTickIntervalSec :: NominalDiffTime
betterDriverSearchProximityTickIntervalSec = 10

checkBetterDriverSearchProximity ::
  ( EsqDBFlow m r,
    EncFlow m r,
    HasHttpClientOptions r c,
    HasShortDurationRetryCfg r c,
    CacheFlow m r,
    HasField "modelNamesHashMap" r (HMS.HashMap Text Text),
    HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    HasFlowEnv m r '["cloudType" ::: Maybe CloudType],
    HasField "s3Env" r (S3.S3Env m),
    LT.HasLocationService m r,
    HasFlowEnv m r '["ondcTokenHashMap" ::: HMS.HashMap KeyConfig TokenConfig],
    HasFlowEnv m r '["internalEndPointHashMap" ::: HMS.HashMap BaseUrl BaseUrl],
    HasFlowEnv m r '["kafkaProducerTools" ::: KafkaProducerTools],
    EsqDBReplicaFlow m r,
    HasField "searchRequestExpirationSeconds" r NominalDiffTime,
    HasField "jobInfoMap" r (M.Map Text Bool),
    HasField "maxShards" r Int,
    HasField "schedulerSetName" r Text,
    HasField "schedulerType" r SchedulerType,
    Metrics.HasSendSearchRequestToDriverMetrics m r,
    Metrics.HasDriverSearchRequestResponseMetrics m r,
    Metrics.HasBPPMetrics m r,
    HasLongDurationRetryCfg r c,
    HasField "singleBatchProcessingTempDelay" r NominalDiffTime,
    TranslateFlow m r,
    HasFlowEnv m r '["maxNotificationShards" ::: Int],
    EventStreamFlow m r,
    Metrics.HasCoreMetrics r,
    HasField "enableAPILatencyLogging" r Bool,
    HasField "enableAPIPrometheusMetricLogging" r Bool,
    HasFlowEnv m r '["appBackendBapInternal" ::: AppBackendBapInternal],
    HasFlowEnv m r '["fabricGatewayBaseUrl" ::: BaseUrl],
    HasField "serviceClickhouseCfg" r CH.ClickhouseCfg,
    HasField "serviceClickhouseEnv" r CH.ClickhouseEnv,
    HasField "blackListedJobs" r [Text],
    HasField "enableLtsPoolDataForPooling" r Bool,
    Redis.HedisLTSFlowEnv r,
    CH.ClickhouseFlow m r,
    Finance.HasActorInfo m r,
    BeamFlow m r,
    CoreMetrics m,
    HasField "driverQuoteExpirationSeconds" r NominalDiffTime,
    HasFlowEnv m r '["version" ::: DeploymentVersion],
    HasPrettyLogger m r,
    ServiceFlow m r,
    HasField "quoteRespondCoolDown" r Int,
    HasField "driverUnlockDelay" r Seconds
  ) =>
  Job 'CheckBetterDriverSearchProximity ->
  m ExecutionResult
checkBetterDriverSearchProximity Job {id, jobInfo} = withLogTag ("JobId-" <> id.getId) do
  let jobData = jobInfo.jobData
      searchTryId = jobData.searchTryId
      bookingId = jobData.bookingId
      rideId = jobData.rideId
      driverId = jobData.driverId
      merchantId = jobData.merchantId
      reschedule = do
        now <- getCurrentTime
        return $ ReSchedule (addUTCTime betterDriverSearchProximityTickIntervalSec now)
  mbSearchTry <- QST.findById searchTryId
  case mbSearchTry of
    Nothing -> return $ Terminate "Stand-by search try not found"
    Just searchTry
      | searchTry.status /= DST.ACTIVE -> return Complete -- already resolved (swapped, cancelled, or expired) elsewhere
      | searchTry.searchRepeatType /= DST.BETTER_DRIVER_SEARCH -> return Complete -- promoted to a real reallocation; no longer this job's concern
      | otherwise -> do
        mbBooking <- QBooking.findById bookingId
        case mbBooking of
          Nothing -> return $ Terminate "Booking not found"
          Just booking -> do
            mbTransporterConfig <- getOneConfig (TransporterConfigDimensions {merchantOperatingCityId = booking.merchantOperatingCityId.getId}) Nothing
            case mbTransporterConfig >>= (.betterDriverProximityAbortRadiusMeters) of
              Nothing -> return $ Terminate "No proximity-abort radius configured for this city"
              Just abortRadius -> do
                driverLocationResp <- LTSF.pickupDriverLocation rideId merchantId driverId
                case lastMaybe driverLocationResp.loc of
                  Nothing -> reschedule -- no fresh pickup-leg location yet; try again next tick
                  Just latest -> do
                    let pickupLoc = getCoordinates booking.fromLocation
                        distance = distanceBetweenInMeters (LatLong latest.lat latest.lon) pickupLoc
                    if distance < abortRadius
                      then do
                        QST.cancelBetterDriverSearchByBookingId bookingId.getId
                        return Complete
                      else reschedule
  where
    lastMaybe [] = Nothing
    lastMaybe xs = Just (last xs)
