{-# LANGUAGE TypeApplications #-}

module SharedLogic.SharedCab.JourneyLeg
  ( getSharedCabLegState,
    noBookingLegState,
  )
where

import qualified API.Types.UI.MultimodalConfirm as APITypes
import qualified BecknV2.FRFS.Enums as Spec
import Domain.Types.FRFSRouteDetails (gtfsIdtoDomainCode)
import Domain.Types.FRFSSearch (FRFSSearch)
import qualified Domain.Types.FRFSTicketBooking as DFRFSTicketBooking
import qualified Domain.Types.FRFSTicketStatus as DFRFSTicket
import qualified Domain.Types.JourneyLeg as DJourneyLeg
import qualified Domain.Types.Trip as DTrip
import Kernel.External.Maps.Types (LatLong)
import Kernel.Prelude
import Kernel.Storage.Hedis as Redis
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.JourneyLeg.Types as JLTypes
import qualified Lib.JourneyModule.State.Types as JMStateTypes
import qualified Lib.JourneyModule.Types as JT
import qualified SharedLogic.SharedCab.Allocation as SharedCabAllocation
import SharedLogic.SharedCab.Allocation.Types (AllocationState (..), RiderFix (..))
import qualified SharedLogic.SharedCab.Booking as SharedCabBooking
import qualified SharedLogic.SharedCab.Config as Config
import qualified SharedLogic.SharedCab.Degraded as SharedCabDegraded
import qualified SharedLogic.SharedCab.Events as SharedCabEvents
import qualified SharedLogic.SharedCab.Keys as Keys
import qualified SharedLogic.SharedCab.LegState as SharedCabLeg
import qualified SharedLogic.SharedCab.Session as SharedCabSession
import qualified SharedLogic.SharedCab.SessionState as SharedCabSessionState
import qualified Storage.Queries.JourneyLeg as QJourneyLeg

getSharedCabLegState ::
  (CacheFlow m r, EsqDBFlow m r, Redis.HedisFlow m r, Redis.HedisLTSFlowEnv r, MonadFlow m, SharedCabEvents.EventFlow m r) =>
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
  mbSession <- maybe (pure Nothing) SharedCabSession.readSession booking.vehicleNumber
  cabsComing <- maybe (pure 0) (fmap length . SharedCabSession.activeSessionsOnRoute) mbRouteCode
  -- the fallback gate and the arrival deadline both live in the cross-app Redis cell the allocation tick writes to
  cfg <- Config.getTunables booking.merchantOperatingCityId
  findingSince <- SharedCabAllocation.readFindingSince booking.id booking.createdAt
  attempts <- SharedCabBooking.shared $ fromMaybe 0 <$> Redis.safeGet (Keys.attemptsKey booking.id)
  mbAllocState <- SharedCabBooking.shared $ Redis.safeGet @AllocationState (Keys.allocKey booking.id)
  -- the cab's fix and ETAs: one read of the route's LTS hash (what the allocator reads); any miss is Nothing
  mbCabTracking <- case mbSession of
    Just session
      | session.status /= SharedCabSessionState.ENDED ->
        withTryCatch "sharedCab:legState:readCabTracking" (SharedCabAllocation.readRoutePosition session.routeCode session.vehicleNumber)
          <&> either (const Nothing) identity
    _ -> pure Nothing
  mbDriverInfo <- case mbSession of
    Just session | session.status /= SharedCabSessionState.ENDED -> SharedCabSession.readCabDriverInfo session.vehicleNumber
    _ -> pure Nothing
  let cabTrack = do
        veh <- mbCabTracking
        guard (SharedCabAllocation.isFreshPosition now cfg.ltsMaxAgeSec veh.vehicleInfo)
        pure (SharedCabLeg.cabTracking now booking.fromStationCode booking.toStationCode veh.vehicleInfo)
  let hasLiveSession = maybe False ((/= SharedCabSessionState.ENDED) . (.status)) mbSession
      (trackingStatus, trackingStatusLastUpdatedAt) = maybe (JMStateTypes.InPlan, now) (\(_, ts, tsAt) -> (ts, tsAt)) (listToMaybe trackingStatuses)
      arrivalDeadline = mbAllocState >>= (.expiresAt)
      mbState = SharedCabLeg.deriveSharedCabState now bookingStatus' booking.vehicleNumber hasLiveSession (Just fallbackGate) arrivalDeadline
      boardDeadlineSec = guard (mbState == Just SharedCabLeg.ARRIVING) *> arrivalDeadline <&> \deadline -> max 0 (ceiling (diffUTCTime deadline now) :: Int)
      fallbackGate =
        SharedCabLeg.FallbackGate
          { attempts,
            maxAttempts = cfg.maxAttempts,
            findingSince,
            fallbackAfterSec = cfg.fallbackAfterSec
          }
      mkSharedCab reason st =
        let live = if st `elem` [SharedCabLeg.DROPPED, SharedCabLeg.CANCELLED] then Nothing else cabTrack
         in SharedCabLeg.SharedCabLegStatus
              { state = st,
                bookingId = booking.id.getId,
                vehicleNumber = booking.vehicleNumber,
                vehicleModel = mbDriverInfo >>= (.vehicleModel),
                driverName = mbDriverInfo >>= (.driverName),
                driverPhotoUrl = Nothing,
                etaToBoardStopSec = live >>= (\(_, board, _) -> board),
                etaToDropStopSec = live >>= (\(_, _, dropEta) -> dropEta),
                position = (\(pos, _, _) -> pos) <$> live,
                cabsComing,
                boardDeadlineSec,
                cancelReason = reason
              }
  -- where the rider is while a cab is on its way decides the blame if it passes the stop
  when (mbState `elem` [Just SharedCabLeg.ALLOCATED, Just SharedCabLeg.ARRIVING]) $
    whenJust (listToMaybe riderLastPoints) $ \p -> SharedCabBooking.recordRiderFix booking.id RiderFix {position = p.latLong, takenAt = p.currTime}
  -- why a cancelled leg was cancelled (written by the cancel paths; absent once its TTL lapses)
  mbCancelReason <-
    if mbState == Just SharedCabLeg.CANCELLED
      then SharedCabBooking.readCancelReason booking.id
      else pure Nothing
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
          sharedCab = mkSharedCab mbCancelReason <$> mbState
        }
  where
    mbRouteCode = listToMaybe journeyLeg.routeDetails >>= (.routeGtfsId) <&> gtfsIdtoDomainCode

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
