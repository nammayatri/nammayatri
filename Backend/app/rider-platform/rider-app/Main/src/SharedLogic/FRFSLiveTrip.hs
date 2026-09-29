module SharedLogic.FRFSLiveTrip
  ( LiveTripDecision (..),
    getLiveTripDecision,
  )
where

import qualified BecknV2.FRFS.Enums as Spec
import Data.Aeson
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import qualified Domain.Types.FRFSTicketBooking as DFRFSTicketBooking
import qualified Domain.Types.JourneyLeg as DJourneyLeg
import Kernel.External.Types (ServiceFlow)
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Version (CloudType (..))
import Kernel.Utils.Common
import qualified Lib.JourneyModule.Utils as JourneyUtils
import qualified SharedLogic.FRFSUtils as FRFSUtils
import qualified SharedLogic.IntegratedBPPConfig as SIBC
import qualified Storage.CachedQueries.FRFSVehicleServiceTier as CQFRFSVehicleServiceTier
import qualified Storage.CachedQueries.Merchant.MultiModalBus as CQMMB
import qualified Storage.CachedQueries.OTPRest.OTPRest as OTPRest

data LiveTripDecision = LiveTripDecision
  { canCancel :: Bool,
    cancelDenyReason :: Maybe Text,
    fullRefund :: Bool,
    chargeAnchor :: UTCTime,
    canReschedule :: Bool,
    rescheduleDenyReason :: Maybe Text
  }
  deriving (Show, Eq, Generic, ToJSON)

getLiveTripDecision ::
  (ServiceFlow m r, HasShortDurationRetryCfg r c, Hedis.HedisLTSFlowEnv r, HasField "cloudType" r (Maybe CloudType)) =>
  DFRFSTicketBooking.FRFSTicketBooking ->
  m (Maybe LiveTripDecision)
getLiveTripDecision booking = do
  eDecision <- withTryCatch "FRFSLiveTrip:getLiveTripDecision" $ do
    now <- getCurrentTime
    case (booking.vehicleType, booking.tripId, booking.routeCode) of
      (Spec.BUS, Just tripId, Just routeCode) -> do
        integratedBppConfig <- SIBC.findIntegratedBPPConfigFromEntity booking
        let (waybillNo, tripNo) = JourneyUtils.getWaybillNoAndTripNoFromTripId tripId
        schedule <- OTPRest.getBusTripSchedule waybillNo tripNo routeCode integratedBppConfig
        let allEtas = concatMap (.eta) schedule
        let tripOverReason = "This trip has already been completed"
        case find (\e -> e.stopCode == booking.fromStationCode) allEtas of
          Nothing
            | null schedule ->
              pure $
                Just
                  LiveTripDecision
                    { canCancel = False,
                      cancelDenyReason = Just tripOverReason,
                      fullRefund = False,
                      chargeAnchor = now,
                      canReschedule = False,
                      rescheduleDenyReason = Just tripOverReason
                    }
            | otherwise -> pure Nothing
          Just boardingEta -> do
            mbVst <- fmap join $
              forM (FRFSUtils.getServiceTierTypeFromRouteStationsJson booking.routeStationsJson) $ \serviceTierType ->
                CQFRFSVehicleServiceTier.findByServiceTierAndMerchantOperatingCityIdAndIntegratedBPPConfigId serviceTierType booking.merchantOperatingCityId integratedBppConfig.id
            let scheduledDeparture = FRFSUtils.unixToUTC boardingEta.arrivalTimeUnix
                scheduledTripStart = FRFSUtils.unixToUTC (minimum (map (.arrivalTimeUnix) allEtas))
                scheduledTripEnd = FRFSUtils.unixToUTC (maximum (map (.arrivalTimeUnix) allEtas))
                mbDelayThreshold = mbVst >>= (.cancellationDelayThresholdSeconds)
                windowEnd = addUTCTime (diffUTCTime scheduledTripEnd scheduledTripStart) scheduledDeparture
                mbIsActiveTrip = listToMaybe (mapMaybe (.is_active_trip) schedule)
                shouldCheckLive =
                  fromMaybe False (mbVst >>= (.useLiveForCancellationAndRescheduling))
                    && case mbIsActiveTrip of
                      Just isActive -> isActive
                      Nothing -> False
            (hasLiveMatch, crossedBoardingStop, mbLiveDeparture) <-
              case (if shouldCheckLive then booking.vehicleNumber else Nothing) of
                Nothing -> pure (False, False, Nothing)
                Just vehicleNumber -> do
                  routeWithBuses <- CQMMB.getRoutesBuses routeCode integratedBppConfig
                  let nowSec = floor (utcTimeToPOSIXSeconds now) :: Int
                      mbEtas = do
                        bus <- find (\b -> b.vehicleNumber == vehicleNumber) routeWithBuses.buses
                        guard (nowSec - bus.busData.timestamp <= 300)
                        etas <- bus.busData.eta_data
                        guard (not (null etas))
                        pure etas
                  pure $ case mbEtas of
                    Nothing -> (False, False, Nothing)
                    Just etas -> case find (\e -> e.stopCode == booking.fromStationCode) etas of
                      Just stopEta -> (True, False, Just (FRFSUtils.unixToUTC stopEta.arrivalTimeUnix))
                      Nothing -> (True, True, Nothing)
            let completed = any ((== Just True) . (.is_completed)) schedule
                rescheduleGrace = secondsToNominalDiffTime (fromMaybe (Seconds 1800) (mbVst >>= (.maxRescheduleTimeAfterStart)))
                deviationSecs = case mbLiveDeparture of
                  Just liveDeparture -> round (diffUTCTime liveDeparture scheduledDeparture)
                  Nothing
                    | mbIsActiveTrip == Just True -> 0
                    | isNothing mbDelayThreshold -> 0
                    | otherwise -> max 0 (round (diffUTCTime now scheduledTripStart))
                effectiveDeparture = addUTCTime (fromIntegral deviationSecs) scheduledDeparture
                mbDenyReason
                  | booking.finalBoardedVehicleNumberSource == Just DJourneyLeg.UserActivated = Just "You have already boarded this trip"
                  | completed = Just tripOverReason
                  | hasLiveMatch && crossedBoardingStop = Just "Your bus has already departed from the boarding stop"
                  | now > windowEnd = Just "This trip's scheduled window has passed"
                  | otherwise = Nothing
                rescheduleWindowEnd
                  | Just liveDeparture <- mbLiveDeparture = liveDeparture
                  | mbIsActiveTrip == Just True = addUTCTime rescheduleGrace scheduledDeparture
                  | otherwise = addUTCTime rescheduleGrace scheduledTripEnd
                decision =
                  LiveTripDecision
                    { canCancel = isNothing mbDenyReason,
                      cancelDenyReason = mbDenyReason,
                      fullRefund =
                        isNothing mbDenyReason
                          && maybe False (\threshold -> deviationSecs >= getSeconds threshold) mbDelayThreshold,
                      chargeAnchor = effectiveDeparture,
                      canReschedule = isNothing mbDenyReason && now <= rescheduleWindowEnd,
                      rescheduleDenyReason =
                        case mbDenyReason of
                          Just reason -> Just reason
                          Nothing -> if now <= rescheduleWindowEnd then Nothing else Just "Reschedule window has passed for this booking"
                    }
            logInfo $ "FRFSLiveTrip: bookingId-" <> booking.id.getId <> " deviationSecs=" <> show deviationSecs <> " hasLiveMatch=" <> show hasLiveMatch <> " decision=" <> show decision
            pure (Just decision)
      _ -> pure Nothing
  case eDecision of
    Left err -> do
      logWarning $ "FRFSLiveTrip: no decision for bookingId-" <> booking.id.getId <> ", leaving eligibility to legacy checks: " <> show err
      pure Nothing
    Right mbDecision -> pure mbDecision
