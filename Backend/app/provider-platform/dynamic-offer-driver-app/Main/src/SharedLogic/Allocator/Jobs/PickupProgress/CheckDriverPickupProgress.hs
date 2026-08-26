{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- | Pickup progress monitor: one shared progress clock per ride instead of a per-case
-- state machine. `bestDistance` only improves; `faultSeconds` accumulates whenever fresh
-- location evidence shows no progress (STALLED and MOVING_AWAY alike — switching fault
-- kinds never resets escalation). GPS-dark time is judgment-pending: forgiven if the
-- driver reappears closer than his previous best, counted as fault otherwise; while dark
-- only the gentle non-terminal dark ladder runs. A demonstrably-driving driver who is
-- not (yet) getting closer (U-turns, one-way overshoots) burns a bounded detour credit
-- before his time starts counting as fault.
module SharedLogic.Allocator.Jobs.PickupProgress.CheckDriverPickupProgress where

import qualified AWS.S3 as S3
import qualified Control.Monad.Catch as C
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
import SharedLogic.Allocator.Jobs.ScheduledRides.ScheduledRideAssignedOnUpdate (cancelOrReallocate)
import SharedLogic.BehaviourManagement.PickupStall as PickupStall
import SharedLogic.CallBAPInternal
import qualified SharedLogic.CallInternalMLPricing as ML
import SharedLogic.CancellationConsequence (cityHasDriverCancelMoneyPenalty)
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
    HasFlowEnv m r '["mlPricingInternal" ::: ML.MLPricingInternal],
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
    HasField "driverUnlockDelay" r Seconds,
    C.MonadCatch m
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
      | ride.status == DRide.CANCELLED -> do
        -- the cancel flows flush with the behaviour at cancel time; this is the fallback
        PickupStall.flushPickupJourney ride Nothing
        return Complete
      | ride.status /= DRide.NEW || isJust ride.driverArrivalTime -> do
        PickupStall.flushPickupJourney ride (Just DRide.REACHED_PICKUP)
        return Complete
      | otherwise -> do
        mbTransporterConfig <- getOneConfig (TransporterConfigDimensions {merchantOperatingCityId = ride.merchantOperatingCityId.getId}) Nothing
        case mbTransporterConfig >>= (.pickupStallMonitoringConfig) of
          Nothing -> do
            PickupStall.flushPickupJourney ride Nothing
            return $ Terminate "Pickup stall monitoring is not configured"
          Just cfg -> do
            mbBooking <- QBooking.findById bookingId
            case mbBooking of
              Nothing -> return $ Terminate "Booking not found"
              Just booking -> do
                now <- getCurrentTime
                let rescheduleResult = ReSchedule $ addUTCTime (fromIntegral cfg.tickIntervalSec) now
                    saveState st = Redis.setExp (pickupProgressStateKey rideId) st pickupProgressStateTtl
                mbDriverInfo <- QDI.findById driverId
                mbActiveRide <- QRide.getLatestActiveByDriverId driverId
                -- Forward-batch guard: while the driver is still finishing a previous ride he is
                -- expected to move toward that ride's drop, possibly away from our pickup. His
                -- pickup phase for this ride has not started: keep a clean slate.
                let onAnotherRide = (mbDriverInfo <&> (.onRide)) == Just True && (mbActiveRide <&> (.id)) /= Just rideId
                if onAnotherRide
                  then do
                    saveState emptyPickupProgressState
                    return rescheduleResult
                  else do
                    mbDriverLocation <- do
                      driverLocations <- withTryCatch "driversLocation:checkDriverPickupProgress" $ LTF.driversLocation [driverId]
                      case driverLocations of
                        Left err -> do
                          logWarning $ "driversLocation failed in pickup progress monitor: " <> show err
                          return Nothing
                        Right locations -> return $ listToMaybe locations
                    -- The LTS last-known-location key has a long TTL and is never cleared when a
                    -- driver's GPS goes dark, so an absent key is not the only signal for "dark" —
                    -- a stale ping means we're reading an old position, not a live one.
                    let staleAfter = fromIntegral (fromMaybe (2 * cfg.tickIntervalSec) cfg.staleFixAfterSec) :: NominalDiffTime
                        mbFreshFix =
                          mbDriverLocation >>= \dloc ->
                            if diffUTCTime now dloc.coordinatesCalculatedAt <= staleAfter then Just dloc else Nothing
                    state <- fromMaybe emptyPickupProgressState <$> Redis.safeGet (pickupProgressStateKey rideId)
                    let pickupLoc = LatLong {lat = booking.fromLocation.lat, lon = booking.fromLocation.lon}
                        progressThreshold = fromIntegral $ fromMaybe defaultProgressThresholdMeters cfg.progressThresholdMeters :: Double
                        deviationAllowance = fromIntegral $ fromMaybe defaultDeviationAllowanceMeters cfg.deviationAllowanceMeters :: Double
                        detourDisplacement = fromIntegral $ fromMaybe defaultDetourDisplacementMeters cfg.detourDisplacementMeters :: Double
                        detourCredit = fromMaybe defaultDetourCreditSec cfg.detourCreditSec
                        elapsedSec = maybe cfg.tickIntervalSec (\t -> max 0 . round $ diffUTCTime now t) state.lastTickAt
                    situation <- rideSituation booking
                    case mbFreshFix of
                      Nothing -> do
                        -- Dark tick: judgment pending. The fault clock freezes; only the gentle
                        -- dark ladder (GPS nudges, never terminal) advances.
                        let darkSince' = fromMaybe now state.darkSince
                            darkSpanSec = max 0 . round $ diffUTCTime now darkSince' :: Int
                            state' = state {darkSince = Just darkSince', behaviour = DRide.GPS_DARK, lastTickAt = Just now}
                        logInfo $ pickupTickLog rideId (Nothing :: Maybe Double) state' darkSpanSec
                        case listToMaybe (drop state'.firedDarkStageCount cfg.darkStages) of
                          Just stage | darkSpanSec >= stage.afterDarkSec -> do
                            sendStallNudge ride stage.channel (fromMaybe [] stage.chatSuggestions) (stage.overlayKey <> "_" <> situation)
                            saveState state' {firedDarkStageCount = state'.firedDarkStageCount + 1}
                          _ -> saveState state'
                        return rescheduleResult
                      Just gpsFix -> do
                        let currentDistance = realToFrac $ distanceBetweenInMeters (LatLong gpsFix.lat gpsFix.lon) pickupLoc :: Double
                            madeProgress = maybe True (\best -> currentDistance <= best - progressThreshold) state.bestDistance
                            withFix st =
                              st
                                { lastFixLat = Just gpsFix.lat,
                                  lastFixLon = Just gpsFix.lon,
                                  lastTickAt = Just now,
                                  darkSince = Nothing,
                                  firedDarkStageCount = 0
                                }
                        if madeProgress
                          then do
                            -- Progress (or first baseline fix). Any pending dark span is forgiven —
                            -- he provably drove toward the pickup through it. faultSeconds is the
                            -- lifetime total for this pickup and is deliberately NOT reset.
                            let state' = (withFix state) {bestDistance = Just currentDistance, behaviour = DRide.PROGRESSING}
                            logInfo $ pickupTickLog rideId (Just currentDistance) state' (0 :: Int)
                            saveState state'
                            return rescheduleResult
                          else do
                            -- No progress. A pending dark span is resolved against him: he was at
                            -- bestDistance-or-worse before it and still is, so the whole span counts
                            -- (elapsedSec would only re-count the tail of that span, hence either/or).
                            let darkPenaltySec = maybe 0 (\since -> max 0 . round $ diffUTCTime now since) state.darkSince :: Int
                                displacement = case (state.lastFixLat, state.lastFixLon) of
                                  (Just lastLat, Just lastLon) -> realToFrac $ distanceBetweenInMeters (LatLong lastLat lastLon) (LatLong gpsFix.lat gpsFix.lon) :: Double
                                  _ -> 0
                                -- extra fairness for U-turns/one-ways: a driver demonstrably driving
                                -- (real displacement, judged on consecutive fixes only) pauses the
                                -- clock until the bounded credit runs out
                                isDetour = isNothing state.darkSince && displacement >= detourDisplacement && state.detourCreditUsedSec + elapsedSec <= detourCredit
                                candidateBehaviour
                                  | isDetour = DRide.DETOURING
                                  | maybe False (\best -> currentDistance > best + deviationAllowance) state.bestDistance = DRide.MOVING_AWAY
                                  | otherwise = DRide.STALLED
                                accrualSec = if isDetour then 0 else (if darkPenaltySec > 0 then darkPenaltySec else elapsedSec)
                                state' =
                                  (withFix state)
                                    { behaviour = candidateBehaviour,
                                      faultSeconds = state.faultSeconds + accrualSec,
                                      detourCreditUsedSec = state.detourCreditUsedSec + (if isDetour then elapsedSec else 0)
                                    }
                            logInfo $ pickupTickLog rideId (Just currentDistance) state' accrualSec
                            case listToMaybe (drop state'.firedStageCount cfg.stages) of
                              Just stage | state'.faultSeconds >= stage.afterFaultSec -> do
                                sendStallNudge ride stage.channel (fromMaybe [] stage.chatSuggestions) (stage.overlayKey <> "_" <> situation)
                                case stage.terminalAction of
                                  Nothing -> do
                                    saveState state' {firedStageCount = state'.firedStageCount + 1}
                                    return rescheduleResult
                                  Just terminalAction -> do
                                    saveState state' {firedStageCount = state'.firedStageCount + 1}
                                    whenJust mbTransporterConfig $ \transporterConfig ->
                                      PickupStall.recordPickupStall transporterConfig driverId ride.merchantOperatingCityId rideId (behaviourLabel candidateBehaviour) (if terminalAction == DTC.REALLOCATE_RIDE then PickupStall.SystemReallocation else PickupStall.SystemDetection)
                                    PickupStall.flushPickupJourney ride Nothing
                                    if terminalAction == DTC.REALLOCATE_RIDE
                                      then do
                                        cancelOrReallocate ride ("Ride is Reallocated because driver did not proceed to pickup (" <> behaviourLabel candidateBehaviour <> ")") True (RideCancel.ApplicationRequestorId id.getId)
                                        return $ Terminate "Ride reallocated due to no pickup progress"
                                      else return $ Terminate "Pickup stall recorded; monitoring stopped"
                              _ -> do
                                saveState state'
                                return rescheduleResult
  where
    pickupTickLog rideId mbCurrentDistance st accrualSec =
      "PickupProgressTick rideId=" <> rideId.getId
        <> " behaviour="
        <> behaviourLabel st.behaviour
        <> " currentDistance="
        <> show mbCurrentDistance
        <> " bestDistance="
        <> show st.bestDistance
        <> " faultSeconds="
        <> show st.faultSeconds
        <> " accruedThisTick="
        <> show accrualSec
        <> " detourCreditUsedSec="
        <> show st.detourCreditUsedSec
        <> " darkSince="
        <> show st.darkSince

    -- Delivery channel per stage: the classic full-screen overlay, or a system chat
    -- message rendered in the ride chat thread with auto-played audio (copy + audio
    -- resolved per driver language from merchant_push_notification).
    sendStallNudge ride channel suggestions nudgeKey =
      case fromMaybe DTC.OVERLAY channel of
        DTC.OVERLAY -> sendStallOverlay ride.merchantOperatingCityId ride.driverId nudgeKey
        DTC.CHAT_MESSAGE -> sendStallChatMessage ride suggestions nudgeKey

    sendStallChatMessage ride suggestions nudgeKey = do
      mbDriver <- QP.findById ride.driverId
      whenJust mbDriver $ \driver ->
        TN.sendSystemChatMessage ride.merchantOperatingCityId driver nudgeKey ride.id suggestions

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
rideSituation :: (CacheFlow m r, EsqDBFlow m r) => DRB.Booking -> m Text
rideSituation booking
  | booking.fareParams.driverCancellationNotAllowed == Just True = pure situationNonCancellable
  | otherwise = do
    feeApplies <- cityHasDriverCancelMoneyPenalty booking.merchantOperatingCityId booking.estimatedFare
    pure $ if feeApplies then situationFeeApplies else situationFreeCancel
