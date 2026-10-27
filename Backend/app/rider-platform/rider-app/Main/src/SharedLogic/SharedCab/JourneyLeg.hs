{-# LANGUAGE TypeApplications #-}

module SharedLogic.SharedCab.JourneyLeg
  ( getSharedCabLegState,
    liveCabLegCities,
    noBookingLegState,
  )
where

import qualified API.Types.UI.MultimodalConfirm as APITypes
import qualified BecknV2.FRFS.Enums as Spec
import Data.List (nub)
import qualified Data.Text as T
import Domain.Types.FRFSRouteDetails (gtfsIdtoDomainCode)
import Domain.Types.FRFSSearch (FRFSSearch)
import qualified Domain.Types.FRFSTicketBooking as DFRFSTicketBooking
import qualified Domain.Types.FRFSTicketStatus as DFRFSTicket
import qualified Domain.Types.JourneyLeg as DJourneyLeg
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Trip as DTrip
import Kernel.External.Maps.Types (LatLong)
import Kernel.Prelude
import Kernel.Storage.Hedis as Redis
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.JourneyLeg.Types as JLTypes
import qualified Lib.JourneyModule.State.Types as JMStateTypes
import qualified Lib.JourneyModule.Types as JT
import qualified SharedLogic.SharedCab.Allocation.Select as Select
import SharedLogic.SharedCab.Allocation.Types (RiderFix (..), allocationDeadline, visiblePlate)
import qualified SharedLogic.SharedCab.Approach as Approach
import qualified SharedLogic.SharedCab.Booking as SharedCabBooking
import SharedLogic.SharedCab.BookingState (readAlloc, readAttempts, readFindingSince, readPreferenceDropped, readPreferred, readRiderSkipUsed)
import qualified SharedLogic.SharedCab.Call as SharedCabCall
import qualified SharedLogic.SharedCab.Config as Config
import qualified SharedLogic.SharedCab.Degraded as SharedCabDegraded
import qualified SharedLogic.SharedCab.Events as SharedCabEvents
import qualified SharedLogic.SharedCab.LegState as SharedCabLeg
import qualified SharedLogic.SharedCab.Lts as Lts
import SharedLogic.SharedCab.Plate (samePlate)
import qualified SharedLogic.SharedCab.Seats as SharedCabSeats
import qualified SharedLogic.SharedCab.Session as SharedCabSession
import qualified SharedLogic.SharedCab.SessionState as SharedCabSessionState
import qualified Storage.Queries.JourneyLeg as QJourneyLeg

type LegFlow m r = (CacheFlow m r, EsqDBFlow m r, Redis.HedisFlow m r, Redis.HedisLTSFlowEnv r, MonadFlow m, SharedCabEvents.EventFlow m r, HasShortDurationRetryCfg r ())

-- | The cities where a polled journey status shows a shared-cab leg that is not over (see 'SharedCabLeg.legNeedsChains').
liveCabLegCities :: [JT.JourneyLegState] -> [Id DMOC.MerchantOperatingCity]
liveCabLegCities legs = nub [d.merchantOperatingCityId | d <- datas, Just cab <- [d.sharedCab], SharedCabLeg.legNeedsChains cab.state]
  where
    datas = concatMap (\case JT.Single d -> [d]; JT.Transit ds -> ds) legs

type CabTrack = (SharedCabLeg.CabPosition, Maybe Int, Maybe Int)

getSharedCabLegState ::
  LegFlow m r =>
  UTCTime ->
  [APITypes.RiderLocationReq] ->
  DJourneyLeg.JourneyLeg ->
  DTrip.MultimodalTravelMode ->
  DFRFSTicketBooking.FRFSTicketBooking ->
  JLTypes.JourneyLegStatus ->
  JMStateTypes.JourneyBookingStatus ->
  [(Int, JMStateTypes.TrackingStatus, UTCTime)] ->
  m JT.JourneyLegState
getSharedCabLegState now riderLastPoints journeyLeg mode booking oldStatus bookingStatus trackingStatuses = do
  -- timeout end: a degraded ride has no tick watching it, so the rider's poll is a clock. If the marker expired this poll flips
  -- INPROGRESS -> USED, and the just-read status is corrected locally so this response already shows DROPPED.
  degradeExpired <- case bookingStatus of
    JMStateTypes.FRFSTicket DFRFSTicket.INPROGRESS -> SharedCabDegraded.expireDegradedBoardingIfNeeded booking
    _ -> pure False
  let bookingStatus' = if degradeExpired then JMStateTypes.FRFSTicket DFRFSTicket.USED else bookingStatus
  mbAllocState <- readAlloc booking.id
  -- an offer the driver has not accepted is not the rider's cab yet: the leg stays FINDING and shows no cab
  let shown = visiblePlate mbAllocState booking.vehicleNumber
      visibleBooking = booking {DFRFSTicketBooking.vehicleNumber = shown}
  mbSession <- maybe (pure Nothing) SharedCabSession.readSession shown
  cabsComing <- maybe (pure 0) (fmap length . SharedCabSession.activeSessionsOnRoute) mbRouteCode
  -- the fallback gate and the arrival deadline both live in the cross-app Redis cell the allocation tick writes to
  cfg <- Config.getTunables booking.merchantOperatingCityId
  findingSince <- readFindingSince booking.id booking.createdAt
  attempts <- readAttempts booking.id
  preference <- (,) . fmap (T.takeEnd 4) <$> readPreferred booking.id <*> readPreferenceDropped booking.id
  cabTrack <- readCabTrack now cfg booking mbSession
  mbDriverInfo <- readLiveDriverInfo mbSession
  let hasLiveSession = maybe False ((/= SharedCabSessionState.ENDED) . (.status)) mbSession
      (trackingStatus, trackingStatusLastUpdatedAt) = maybe (JMStateTypes.InPlan, now) (\(_, ts, tsAt) -> (ts, tsAt)) (listToMaybe trackingStatuses)
      arrivalDeadline = mbAllocState >>= allocationDeadline
      mbState = SharedCabLeg.deriveSharedCabState now bookingStatus' shown hasLiveSession (Just fallbackGate) arrivalDeadline
      boardDeadlineSec = guard (mbState == Just SharedCabLeg.ARRIVING) *> arrivalDeadline <&> \deadline -> max 0 (ceiling (diffUTCTime deadline now) :: Int)
      fallbackGate =
        SharedCabLeg.FallbackGate
          { attempts,
            maxAttempts = cfg.maxAttempts,
            findingSince,
            fallbackAfterSec = cfg.fallbackAfterSec
          }
  let onThisTrip session = session.status /= SharedCabSessionState.ENDED && SharedCabLeg.tripMatches (getId <$> booking.vehicleTripId) session.vehicleTripId.getId
      inWindow = maybe False onThisTrip mbSession && maybe False SharedCabLeg.callWindowState mbState
  maskedCall <- if cfg.callEnabled && inWindow then SharedCabCall.maskedCallNumber booking.merchantOperatingCityId else pure Nothing
  let chatChannel = if cfg.chatEnabled && inWindow then Just booking.id.getId else Nothing
  -- where the rider is while a cab is on its way decides the blame if it passes the stop
  when (mbState `elem` [Just SharedCabLeg.ALLOCATED, Just SharedCabLeg.ARRIVING]) $
    whenJust (listToMaybe riderLastPoints) $ \p -> SharedCabBooking.recordRiderFix booking.id RiderFix {position = p.latLong, takenAt = p.currTime}
  -- why a cancelled leg was cancelled (written by the cancel paths; absent once its TTL lapses)
  mbCancelReason <-
    if mbState == Just SharedCabLeg.CANCELLED
      then SharedCabBooking.readCancelReason booking.id
      else pure Nothing
  -- the assigned cab's own ETA (from the track already read) decides the skip button and which other cabs come after it
  skipUsed <- readRiderSkipUsed booking.id
  let assignedEtaSec = cabTrack >>= \(_, boardEta, _) -> boardEta
      approaching = mbState `elem` [Just SharedCabLeg.ALLOCATED, Just SharedCabLeg.ARRIVING]
      skipAvailable = maybe False (\st -> SharedCabLeg.skipAvailableFor st skipUsed assignedEtaSec) mbState
  upcomingCabs <- case (shown, mbRouteCode) of
    (Just plate, Just routeCode) | approaching -> do
      sessions <- SharedCabSession.activeSessionsOnRoute routeCode
      cabs <- fmap catMaybes . forM [s | s <- sessions, not (samePlate plate s.vehicleNumber)] $ \s ->
        Approach.plausibleEtaToStopSec now cfg routeCode s.vehicleNumber booking.fromStationCode >>= \case
          Nothing -> pure Nothing
          Just etaSec -> do
            freeSeats <- SharedCabSeats.freeForStops s booking.fromStationCode booking.toStationCode
            pure (Just SharedCabLeg.UpcomingCab {cabId = s.vehicleTripId.getId, etaSec, freeSeats})
      pure (SharedCabLeg.upcomingCabsOf assignedEtaSec cabs)
    _ -> pure []
  pure $
    JT.Single
      JT.JourneyLegStateData
        { status = oldStatus,
          bookingStatus = bookingStatus',
          trackingStatus,
          trackingStatusLastUpdatedAt,
          userPosition = (.latLong) <$> listToMaybe riderLastPoints,
          vehiclePositions = [],
          legOrder = journeyLeg.sequenceNumber,
          subLegOrder = 1,
          mode,
          fleetNo = journeyLeg.finalBoardedBusNumber,
          serviceTierType = Just Spec.SHARED_CAB,
          merchantOperatingCityId = booking.merchantOperatingCityId,
          sharedCab = mkLegStatus visibleBooking mbDriverInfo cabsComing boardDeadlineSec cabTrack maskedCall chatChannel mbCancelReason preference (upcomingCabs, skipAvailable) <$> mbState
        }
  where
    mbRouteCode = listToMaybe journeyLeg.routeDetails >>= (.routeGtfsId) <&> gtfsIdtoDomainCode

-- | The live session's cab fix and ETAs, from one read of the route's LTS hash (what the allocator reads); any miss is Nothing.
readCabTrack :: LegFlow m r => UTCTime -> Config.SharedCabTunables -> DFRFSTicketBooking.FRFSTicketBooking -> Maybe SharedCabSessionState.Session -> m (Maybe CabTrack)
readCabTrack now cfg booking = \case
  Just session
    | session.status /= SharedCabSessionState.ENDED -> do
      mbCabTracking <-
        withTryCatch "sharedCab:legState:readCabTracking" (Lts.readRoutePosition session.routeCode session.vehicleNumber)
          <&> either (const Nothing) identity
      pure $ do
        veh <- mbCabTracking
        guard (Select.isFreshPosition now cfg.ltsMaxAgeSec veh.vehicleInfo)
        pure (SharedCabLeg.cabTracking now booking.fromStationCode booking.toStationCode veh.vehicleInfo)
  _ -> pure Nothing

readLiveDriverInfo :: LegFlow m r => Maybe SharedCabSessionState.Session -> m (Maybe SharedCabSession.CabDriverInfo)
readLiveDriverInfo = \case
  Just session | session.status /= SharedCabSessionState.ENDED -> SharedCabSession.readCabDriverInfo session.vehicleNumber
  _ -> pure Nothing

-- | A finished leg shows no live cab.
mkLegStatus ::
  DFRFSTicketBooking.FRFSTicketBooking ->
  Maybe SharedCabSession.CabDriverInfo ->
  Int ->
  Maybe Int ->
  Maybe CabTrack ->
  Maybe Text ->
  Maybe Text ->
  Maybe SharedCabLeg.CancelReason ->
  (Maybe Text, Bool) ->
  ([SharedCabLeg.UpcomingCab], Bool) ->
  SharedCabLeg.SharedCabState ->
  SharedCabLeg.SharedCabLegStatus
mkLegStatus booking mbDriverInfo cabsComing boardDeadlineSec cabTrack maskedCall chatChannel reason (preferredCab, preferredCabLost) (upcomingCabs, skipAvailable) st =
  SharedCabLeg.SharedCabLegStatus
    { state = st,
      bookingId = booking.id.getId,
      vehicleNumber = booking.vehicleNumber,
      vehicleModel = mbDriverInfo >>= (.vehicleModel),
      driverName = mbDriverInfo >>= (.driverName),
      driverPhotoUrl = Nothing,
      etaToBoardStopSec = live >>= (\(_, board, _) -> board),
      etaToDropStopSec = live >>= (\(_, _, dropEta) -> dropEta),
      position = (\(pos, _, _) -> pos) <$> live,
      maskedCallNumber = maskedCall,
      chatChannelId = chatChannel,
      cabsComing,
      boardDeadlineSec,
      cancelReason = reason,
      preferredCab,
      preferredCabLost,
      upcomingCabs,
      skipAvailable
    }
  where
    live = if st `elem` [SharedCabLeg.DROPPED, SharedCabLeg.CANCELLED] then Nothing else cabTrack

noBookingLegState ::
  (CacheFlow m r, EsqDBFlow m r, MonadFlow m) =>
  UTCTime ->
  Maybe LatLong ->
  DJourneyLeg.JourneyLeg ->
  DTrip.MultimodalTravelMode ->
  Id FRFSSearch ->
  JLTypes.JourneyLegStatus ->
  JMStateTypes.JourneyBookingStatus ->
  [(Int, JMStateTypes.TrackingStatus, UTCTime)] ->
  m JT.JourneyLegState
noBookingLegState now userPosition journeyLeg mode searchId oldStatus bookingStatus trackingStatuses = do
  mbCurrentLegDetails <- QJourneyLeg.findByLegSearchId (Just searchId.getId)
  let (trackingStatus, trackingStatusLastUpdatedAt) =
        case listToMaybe trackingStatuses of
          Just (_, ts, tsupAt) -> (ts, tsupAt)
          Nothing -> (JMStateTypes.InPlan, now)
  return $
    JT.Single
      JT.JourneyLegStateData
        { status = oldStatus,
          bookingStatus,
          trackingStatus,
          trackingStatusLastUpdatedAt,
          userPosition,
          JT.vehiclePositions = [],
          legOrder = journeyLeg.sequenceNumber,
          subLegOrder = 1,
          mode,
          fleetNo = mbCurrentLegDetails >>= (.finalBoardedBusNumber),
          serviceTierType = mbCurrentLegDetails >>= (.finalBoardedBusServiceTierType),
          merchantOperatingCityId = journeyLeg.merchantOperatingCityId,
          sharedCab = Nothing
        }
