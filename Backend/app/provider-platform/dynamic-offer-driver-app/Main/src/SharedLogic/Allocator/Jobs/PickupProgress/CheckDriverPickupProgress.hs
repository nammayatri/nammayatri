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
import qualified Domain.Types.Trip as DTrip
import Kernel.External.Maps.Interface.Types
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
import SharedLogic.Allocator.Jobs.ScheduledRides.ScheduledRideAssignedOnUpdate (Result (..), cancelOrReallocate, errorCatchAndHandle)
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
import qualified Tools.Maps as TMaps
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
    HasFlowEnv m r '["fabricGatewayBaseUrl" ::: BaseUrl],
    HasFlowEnv m r '["internalEndPointHashMap" ::: HMS.HashMap BaseUrl BaseUrl],
    HasFlowEnv m r '["kafkaProducerTools" ::: KafkaProducerTools],
    EsqDBReplicaFlow m r,
    HasField "searchRequestExpirationSeconds" r NominalDiffTime,
    HasField "jobInfoMap" r (M.Map Text Bool),
    HasField "maxShards" r Int,
    HasField "schedulerSetName" r Text,
    HasField "schedulerType" r SchedulerType,
    Metrics.HasSendSearchRequestToDriverMetrics m r,
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
        case (mbTransporterConfig, mbTransporterConfig >>= (.pickupStallMonitoringConfig)) of
          (Just transporterConfig, Just monitoringConfig) -> do
            mbBooking <- QBooking.findById bookingId
            case mbBooking of
              Nothing -> return $ Terminate "Booking not found"
              Just booking -> do
                -- ETA feasibility only makes sense for scheduled rides (pickup = future startTime); ad-hoc runs distance only.
                let etaEnabled = booking.isScheduled && isJust monitoringConfig.etaFeasibilityConfig
                    distanceEnabled = not booking.isScheduled || monitoringConfig.runDistanceMonitorForScheduled == Just True
                if not etaEnabled && not distanceEnabled
                  then return $ Terminate "No pickup checks enabled for this ride"
                  else do
                    now <- getCurrentTime
                    let rescheduleResult = ReSchedule $ addUTCTime (fromIntegral monitoringConfig.tickIntervalSec) now
                    mbDriverInfo <- QDI.findById driverId
                    mbActiveRide <- QRide.getLatestActiveByDriverId driverId
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
                        -- The LTS last-known-location key has a long TTL and is never cleared when a
                        -- driver's GPS goes dark, so an absent key is not the only signal for "dark" —
                        -- a ping older than 2 ticks means we're reading a stale position, not a live one.
                        let staleAfter = fromIntegral (2 * monitoringConfig.tickIntervalSec) :: NominalDiffTime
                            mbFreshDriverLocation =
                              mbDriverLocation >>= \dloc ->
                                if diffUTCTime now dloc.coordinatesCalculatedAt <= staleAfter then Just dloc else Nothing
                        state0 <- fromMaybe emptyPickupProgressState <$> Redis.safeGet (pickupProgressStateKey rideId)
                        let situation = rideSituation booking
                            sendMonitorOverlay overlayKey = sendStallOverlay ride.merchantOperatingCityId driverId (overlayKey <> "_" <> situation)
                            -- ad-hoc reallocation stays app-attributed; scheduled stays merchant-attributed (unchanged per path).
                            requestor = if booking.isScheduled then RideCancel.MerchantRequestorId (booking.providerId, ride.merchantOperatingCityId) else RideCancel.ApplicationRequestorId id.getId
                            -- Terminal action scopes reallocation by ride kind (InterCity/Rental are advance bookings but
                            -- excluded from "scheduled"); category must also be reallocatable, else warn-only.
                            isInterCityOrRental = DTrip.isInterCityTrip booking.tripCategory || DTrip.isRentalTrip booking.tripCategory
                            shouldReallocate action = terminalActionReallocates action booking.isScheduled isInterCityOrRental && DTrip.isReallocatableCategory booking.tripCategory
                        -- ETA feasibility (scheduled), re-checked every tick: A = now + OSRM duration. Warn while pickup < A <= pickup+grace;
                        -- when A > pickup+grace reallocate after a 2-tick debounce (badTickDebounce) if the terminal action authorizes it, else keep warning.
                        let runEtaTick state =
                              case (mbFreshDriverLocation, monitoringConfig.etaFeasibilityConfig) of
                                (Just dloc, Just etaCaseConfig) -> do
                                  let req =
                                        TMaps.GetDistanceReq
                                          { origin = LatLong {lat = dloc.lat, lon = dloc.lon},
                                            destination = LatLong {lat = booking.fromLocation.lat, lon = booking.fromLocation.lon},
                                            travelMode = Just TMaps.CAR,
                                            distanceUnit = Meter,
                                            sourceDestinationMapping = Nothing
                                          }
                                  responseArray <- errorCatchAndHandle [req] (TMaps.getDistanceForScheduledRides booking.providerId ride.merchantOperatingCityId (Just ride.id.getId))
                                  if any isApiFailure responseArray
                                    then do
                                      -- Continuous monitor: a transient maps failure just skips this tick, never fail-closed reallocate.
                                      logWarning "pickup monitor ETA: getDistance failed; skipping tick"
                                      return (state, Nothing)
                                    else do
                                      let durationSecs = foldr accumulateDuration 0 responseArray
                                          expectedArrival = addUTCTime (secondsToNominalDiffTime durationSecs) now
                                          pickupTime = booking.startTime
                                          breachAt = addUTCTime transporterConfig.graceTimeForScheduledRidePickup pickupTime
                                          mbHeadStage = listToMaybe etaCaseConfig.stages
                                          etaAction = mbHeadStage >>= (.terminalAction)
                                          etaOverlayKey = maybe "SCHEDULED_ETA_RISK" (.overlayKey) mbHeadStage
                                      if expectedArrival > breachAt
                                        then do
                                          let breaches = state.consecutiveEtaBreaches + 1
                                          if breaches >= monitoringConfig.badTickDebounce && maybe False shouldReallocate etaAction
                                            then do
                                              Redis.del (pickupProgressStateKey rideId)
                                              cancelOrReallocate ride "Ride is Reallocated because driver can't reach the scheduled pickup on time (ETA)" True requestor
                                              return (state, Just (Terminate "Ride reallocated due to ETA infeasibility"))
                                            else do
                                              sendMonitorOverlay etaOverlayKey
                                              return (state {consecutiveEtaBreaches = breaches}, Nothing)
                                        else do
                                          when (expectedArrival > pickupTime) $ sendMonitorOverlay etaOverlayKey
                                          return (state {consecutiveEtaBreaches = 0}, Nothing)
                                _ -> return (state, Nothing)
                        -- Distance progress: shared classification; reallocation scope + Behaviour Engine gated by ride kind.
                        let runDistanceTick state = do
                              let pickupLoc = LatLong {lat = booking.fromLocation.lat, lon = booking.fromLocation.lon}
                                  mbCurrentDistance = mbFreshDriverLocation <&> \dloc -> realToFrac $ distanceBetweenInMeters (LatLong dloc.lat dloc.lon) pickupLoc
                                  progressThreshold = fromIntegral monitoringConfig.progressThresholdMeters
                                  tickCase = classifyTick state.lastDistanceToPickup mbCurrentDistance progressThreshold
                              logInfo $
                                "PickupProgressTick rideId=" <> rideId.getId
                                  <> " isScheduled="
                                  <> show booking.isScheduled
                                  <> " currentDistance="
                                  <> show mbCurrentDistance
                                  <> " tickCase="
                                  <> show tickCase
                                  <> " activeCaseBefore="
                                  <> show state.activeCase
                                  <> " firedStageCountBefore="
                                  <> show state.firedStageCount
                              case tickCase of
                                Nothing -> return (progressResetDistance state (mbCurrentDistance <|> state.lastDistanceToPickup), Nothing)
                                Just detectedCase -> do
                                  let state' = advanceCase monitoringConfig.badTickDebounce detectedCase now state
                                      keepState = state' {lastDistanceToPickup = mbCurrentDistance <|> state'.lastDistanceToPickup}
                                  case state'.activeCase of
                                    Just activeCase' | activeCase' == detectedCase -> do
                                      let stallDuration = maybe 0 (diffUTCTime now) state'.caseStartedAt
                                          stages = stagesForCase monitoringConfig activeCase'
                                          advanced = state' {firedStageCount = state'.firedStageCount + 1, lastDistanceToPickup = mbCurrentDistance <|> state'.lastDistanceToPickup}
                                      case listToMaybe (drop state'.firedStageCount stages) of
                                        Just stage | stallDuration >= fromIntegral stage.afterStallSec -> do
                                          sendMonitorOverlay stage.overlayKey
                                          case stage.terminalAction of
                                            Nothing -> return (advanced, Nothing)
                                            Just action
                                              | shouldReallocate action -> do
                                                -- Behaviour Engine (fees/blocks) + stall tag stay ad-hoc-only.
                                                unless booking.isScheduled $ do
                                                  stampPickupStallTag ride activeCase'
                                                  PickupStall.recordPickupStall transporterConfig driverId ride.merchantOperatingCityId rideId activeCase' PickupStall.SystemReallocation
                                                Redis.del (pickupProgressStateKey rideId)
                                                cancelOrReallocate ride ("Ride is Reallocated because driver did not proceed to pickup (" <> activeCase' <> ")") True requestor
                                                return (state', Just (Terminate "Ride reallocated due to no pickup progress"))
                                              | action == DTC.RECORD_ONLY -> do
                                                unless booking.isScheduled $ do
                                                  stampPickupStallTag ride activeCase'
                                                  PickupStall.recordPickupStall transporterConfig driverId ride.merchantOperatingCityId rideId activeCase' PickupStall.SystemDetection
                                                Redis.del (pickupProgressStateKey rideId)
                                                return (state', Just (Terminate "Pickup stall recorded; monitoring stopped"))
                                              -- blocked reallocate (scheduled+REALLOCATE_RIDE or non-reallocatable category): warn + advance, keep monitoring.
                                              | otherwise -> return (advanced, Nothing)
                                        _ -> return (keepState, Nothing)
                                    _ -> return (keepState, Nothing)
                        etaResult <- if etaEnabled then runEtaTick state0 else return (state0, Nothing)
                        case etaResult of
                          (_, Just terminal) -> return terminal
                          (state1, Nothing) ->
                            if distanceEnabled
                              then do
                                distResult <- runDistanceTick state1
                                case distResult of
                                  (_, Just terminal) -> return terminal
                                  (state2, Nothing) -> do
                                    Redis.setExp (pickupProgressStateKey rideId) state2 pickupProgressStateTtl
                                    return rescheduleResult
                              else do
                                Redis.setExp (pickupProgressStateKey rideId) state1 pickupProgressStateTtl
                                return rescheduleResult
          _ -> return $ Terminate "Pickup stall monitoring is not configured"
  where
    accumulateDuration :: Result a b -> Seconds -> Seconds
    accumulateDuration (DistanceResp resp) acc = acc + resp.duration
    accumulateDuration APIFailed acc = acc

    isApiFailure :: Result a b -> Bool
    isApiFailure APIFailed = True
    isApiFailure _ = False

    -- Reset the distance-progress fields on a good tick; ETA breach counter is managed separately.
    progressResetDistance state newLastDistance =
      state
        { candidateCase = Nothing,
          consecutiveBadTicks = 0,
          activeCase = Nothing,
          caseStartedAt = Nothing,
          firedStageCount = 0,
          lastDistanceToPickup = newLastDistance
        }

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
