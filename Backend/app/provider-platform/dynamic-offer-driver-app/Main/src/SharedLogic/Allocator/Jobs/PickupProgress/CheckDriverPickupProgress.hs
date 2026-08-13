{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.Allocator.Jobs.PickupProgress.CheckDriverPickupProgress where

import qualified AWS.S3 as S3
import Control.Applicative ((<|>))
import qualified Data.HashMap.Strict as HMS
import qualified Data.Map as M
import qualified Domain.Action.UI.Ride.CancelRide as RideCancel
import qualified Domain.Types.Booking as DRB
import qualified Domain.Types.Ride as DRide
import qualified Domain.Types.TransporterConfig as DTC
import Kernel.External.Maps.Types (LatLong (..))
import Kernel.External.Types
import Kernel.Prelude
import qualified Kernel.Storage.Clickhouse.Config as CH
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Streaming.Kafka.Producer.Types (KafkaProducerTools)
import Kernel.Types.Version (CloudType)
import Kernel.Utils.CalculateDistance (distanceBetweenInMeters)
import Kernel.Utils.Common
import Lib.ConfigPilot.Interface.Types (getOneConfig)
import qualified Lib.Finance.Core.Types as Finance
import Lib.Scheduler
import Lib.SessionizerMetrics.Types.Event
import SharedLogic.Allocator
import SharedLogic.Allocator.Jobs.ScheduledRides.ScheduledRideAssignedOnUpdate (cancelOrReallocate)
import SharedLogic.BehaviourManagement.PickupStall as PickupStall
import SharedLogic.CallBAPInternal
import qualified SharedLogic.CallInternalMLPricing as ML
import qualified SharedLogic.External.LocationTrackingService.Flow as LTF
import qualified SharedLogic.External.LocationTrackingService.Types as LT
import SharedLogic.GoogleTranslate (TranslateFlow)
import qualified Storage.CachedQueries.Merchant.Overlay as CMP
import Storage.ConfigPilot.Config.TransporterConfig (TransporterConfigDimensions (..))
import qualified Storage.Queries.Booking as QBooking
import qualified Storage.Queries.DriverInformation as QDI
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.Ride as QRide
import qualified Storage.Queries.Vehicle as QVeh
import qualified Tools.Metrics as Metrics
import qualified Tools.Notifications as TN
import TransactionLogs.Types

checkDriverPickupProgress ::
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
    HasLongDurationRetryCfg r c,
    HasField "singleBatchProcessingTempDelay" r NominalDiffTime,
    TranslateFlow m r,
    HasFlowEnv m r '["maxNotificationShards" ::: Int],
    EventStreamFlow m r,
    Metrics.HasCoreMetrics r,
    HasField "enableAPILatencyLogging" r Bool,
    HasField "enableAPIPrometheusMetricLogging" r Bool,
    HasFlowEnv m r '["appBackendBapInternal" ::: AppBackendBapInternal],
    HasFlowEnv m r '["mlPricingInternal" ::: ML.MLPricingInternal],
    HasField "serviceClickhouseCfg" r CH.ClickhouseCfg,
    HasField "serviceClickhouseEnv" r CH.ClickhouseEnv,
    HasField "blackListedJobs" r [Text],
    HasField "enableLtsPoolDataForPooling" r Bool,
    Redis.HedisLTSFlowEnv r,
    CH.ClickhouseFlow m r,
    Finance.HasActorInfo m r
  ) =>
  Job 'CheckDriverPickupProgress ->
  m ExecutionResult
checkDriverPickupProgress Job {id, jobInfo} = withLogTag ("JobId-" <> id.getId) do
  let jobData = jobInfo.jobData
      rideId = jobData.rideId
      driverId = jobData.driverId
      bookingId = jobData.bookingId
  mbRide <- QRide.findById rideId
  case mbRide of
    Nothing -> return $ Terminate "Ride not found"
    Just ride
      | ride.status /= DRide.NEW -> return Complete
      | isJust ride.driverArrivalTime -> return Complete
      | otherwise -> do
        mbTransporterConfig <- getOneConfig (TransporterConfigDimensions {merchantOperatingCityId = ride.merchantOperatingCityId.getId}) Nothing
        case mbTransporterConfig >>= (.pickupStallMonitoringConfig) of
          Nothing -> return $ Terminate "Pickup stall monitoring is not configured"
          Just monitoringConfig -> do
            mbBooking <- QBooking.findById bookingId
            case mbBooking of
              Nothing -> return $ Terminate "Booking not found"
              Just booking -> do
                now <- getCurrentTime
                let rescheduleResult = ReSchedule $ addUTCTime (fromIntegral monitoringConfig.tickIntervalSec) now
                mbDriverInfo <- QDI.findById driverId
                mbActiveRide <- QRide.getActiveByDriverId driverId
                -- Forward-batch guard: while the driver is still finishing a previous ride they are
                -- expected to move toward that ride's drop, possibly away from our pickup.
                let onAnotherRide = (mbDriverInfo <&> (.onRide)) == Just True && (mbActiveRide <&> (.id)) /= Just rideId
                if onAnotherRide
                  then do
                    Redis.setExp (pickupProgressStateKey rideId) emptyPickupProgressState pickupProgressStateTtl
                    return rescheduleResult
                  else do
                    mbDriverLocation <- do
                      driverLocations <- withTryCatch "driversLocation:checkDriverPickupProgress" $ LTF.driversLocation [driverId]
                      case driverLocations of
                        Left err -> do
                          logWarning $ "driversLocation failed in pickup progress monitor: " <> show err
                          return Nothing
                        Right locations -> return $ listToMaybe locations
                    state <- fromMaybe emptyPickupProgressState <$> Redis.safeGet (pickupProgressStateKey rideId)
                    let pickupLoc = LatLong {lat = booking.fromLocation.lat, lon = booking.fromLocation.lon}
                        mbCurrentDistance = mbDriverLocation <&> \dloc -> realToFrac $ distanceBetweenInMeters (LatLong dloc.lat dloc.lon) pickupLoc
                        progressThreshold = fromIntegral monitoringConfig.progressThresholdMeters
                        tickCase = classifyTick state.lastDistanceToPickup mbCurrentDistance progressThreshold
                    case tickCase of
                      Nothing -> do
                        -- Progressing (or first baseline tick): full reset, clean slate.
                        Redis.setExp (pickupProgressStateKey rideId) (emptyPickupProgressState {lastDistanceToPickup = mbCurrentDistance <|> state.lastDistanceToPickup}) pickupProgressStateTtl
                        return rescheduleResult
                      Just detectedCase -> do
                        let state' = advanceCase monitoringConfig.badTickDebounce detectedCase now state
                        case state'.activeCase of
                          Just activeCase' | activeCase' == detectedCase -> do
                            let stallDuration = maybe 0 (diffUTCTime now) state'.caseStartedAt
                                stages = stagesForCase monitoringConfig activeCase'
                            case listToMaybe (drop state'.firedStageCount stages) of
                              Just stage | stallDuration >= fromIntegral stage.afterStallSec -> do
                                let situation = rideSituation booking
                                sendStallOverlay ride.merchantOperatingCityId driverId (stage.overlayKey <> "_" <> situation)
                                -- REALLOCATE_RIDE acts only on non-cancellable rides; for cancellable
                                -- situations the driver always had the cancel exit, so we only record.
                                let shouldReallocate = stage.terminalAction == Just DTC.REALLOCATE_RIDE && situation == situationNonCancellable
                                if isJust stage.terminalAction
                                  then do
                                    stampPickupStallTag ride activeCase'
                                    whenJust mbTransporterConfig $ \transporterConfig ->
                                      PickupStall.recordPickupStall transporterConfig driverId ride.merchantOperatingCityId rideId activeCase' (if shouldReallocate then PickupStall.SystemReallocation else PickupStall.SystemDetection)
                                    if shouldReallocate
                                      then do
                                        Redis.del (pickupProgressStateKey rideId)
                                        cancelOrReallocate ride ("Ride is Reallocated because driver did not proceed to pickup (" <> activeCase' <> ")") True (RideCancel.ApplicationRequestorId id.getId)
                                        return $ Terminate "Ride reallocated due to no pickup progress"
                                      else do
                                        Redis.del (pickupProgressStateKey rideId)
                                        return $ Terminate "Pickup stall recorded; monitoring stopped"
                                  else do
                                    Redis.setExp (pickupProgressStateKey rideId) (state' {firedStageCount = state'.firedStageCount + 1, lastDistanceToPickup = mbCurrentDistance <|> state'.lastDistanceToPickup}) pickupProgressStateTtl
                                    return rescheduleResult
                              _ -> do
                                Redis.setExp (pickupProgressStateKey rideId) (state' {lastDistanceToPickup = mbCurrentDistance <|> state'.lastDistanceToPickup}) pickupProgressStateTtl
                                return rescheduleResult
                          _ -> do
                            Redis.setExp (pickupProgressStateKey rideId) (state' {lastDistanceToPickup = mbCurrentDistance <|> state'.lastDistanceToPickup}) pickupProgressStateTtl
                            return rescheduleResult
  where
    -- Nothing = progressing / baseline; Just case = bad tick. A legitimate road detour can
    -- temporarily increase straight-line distance, so RETREATING relies on the debounce in
    -- advanceCase before it activates.
    classifyTick :: Maybe Double -> Maybe Double -> Double -> Maybe Text
    classifyTick _ Nothing _ = Just caseLocationDark
    classifyTick Nothing (Just _) _ = Nothing
    classifyTick (Just lastD) (Just currentD) threshold
      | lastD - currentD >= threshold = Nothing
      | currentD - lastD >= threshold = Just caseRetreating
      | otherwise = Just caseStalled

    advanceCase :: Int -> Text -> UTCTime -> PickupProgressState -> PickupProgressState
    advanceCase debounce detectedCase now state
      | state.activeCase == Just detectedCase = state
      | state.candidateCase == Just detectedCase =
        let count = state.consecutiveBadTicks + 1
         in if count >= debounce
              then state {activeCase = Just detectedCase, caseStartedAt = Just now, firedStageCount = 0, candidateCase = Nothing, consecutiveBadTicks = 0}
              else state {consecutiveBadTicks = count}
      | otherwise = state {candidateCase = Just detectedCase, consecutiveBadTicks = 1, activeCase = Nothing, caseStartedAt = Nothing, firedStageCount = 0}

    stagesForCase :: DTC.PickupStallMonitoringConfig -> Text -> [DTC.PickupStallStage]
    stagesForCase monitoringConfig caseName
      | caseName == caseStalled = maybe [] (.stages) monitoringConfig.stalledConfig
      | caseName == caseRetreating = maybe [] (.stages) monitoringConfig.retreatingConfig
      | caseName == caseLocationDark = maybe [] (.stages) monitoringConfig.locationDarkConfig
      | otherwise = []

    stampPickupStallTag ride caseName = do
      let stallTag = mkPickupStallRideTag caseName
          existingTags = fromMaybe [] ride.rideTags
      when (stallTag `notElem` existingTags) $
        QRide.updateRideTags (Just $ stallTag : existingTags) ride.id

    sendStallOverlay merchantOpCityId driverId overlayKey = do
      mbDriver <- QP.findById driverId
      whenJust mbDriver $ \driver -> do
        mbVehicle <- QVeh.findById driverId
        let mbVehicleCategory = mbVehicle >>= (.category)
        mbOverlay <- CMP.findByMerchantOpCityIdPNKeyLangaugeUdfVehicleCategory merchantOpCityId overlayKey (fromMaybe ENGLISH driver.language) Nothing mbVehicleCategory Nothing
        case mbOverlay of
          Just overlay -> TN.sendOverlay merchantOpCityId driver $ TN.mkOverlayReq overlay
          Nothing -> logWarning $ "No merchant_overlay row found for pickup stall overlay key: " <> overlayKey

situationNonCancellable, situationFeeApplies, situationFreeCancel :: Text
situationNonCancellable = "NON_CANCELLABLE"
situationFeeApplies = "FEE_APPLIES"
situationFreeCancel = "FREE_CANCEL"

-- Overlay copy varies by how "expensive" cancelling is for the driver on this ride;
-- full overlay key = <stage.overlayKey>_<situation>, seeded per city and language.
rideSituation :: DRB.Booking -> Text
rideSituation booking
  | booking.fareParams.driverCancellationNotAllowed == Just True = situationNonCancellable
  | isJust booking.fareParams.driverCancellationPenaltyAmount = situationFeeApplies
  | otherwise = situationFreeCancel
