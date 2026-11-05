module Domain.Action.UI.SharedCabInternal
  ( postSharedCabRoutesDemand,
    postSharedCabCabSync,
    postSharedCabSessionOperations,
    postSharedCabBookingOperations,
    getSharedCabBookingCallNumber,
    postSharedCabBookingMessage,
    chatMessageBody,
  )
where

import qualified API.Types.UI.SharedCabInternal as API
import qualified Data.Map.Strict as Map
import Data.Maybe (listToMaybe)
import qualified Data.Text as T
import qualified Domain.Types.FRFSTicketBooking as DFTB
import qualified Domain.Types.FRFSTicketStatus as DFRFSTicket
import qualified Domain.Types.IntegratedBPPConfig as DIBC
import qualified Domain.Types.RiderConfig as DRC
import qualified Domain.Types.RouteStopMapping as DRSM
import Domain.Types.SharedCabSession (CabChange (..), CabSyncResp (..), CabUpdate, SharedCabSeatsResp (..))
import qualified Environment
import EulerHS.Prelude hiding (id)
import qualified Kernel.External.Maps.Google.PolyLinePoints as KEPP
import Kernel.External.Maps.Types (LatLong (..))
import qualified Kernel.Storage.InMem as IM
import Kernel.Types.APISuccess (APISuccess (Success))
import Kernel.Types.Id
import Kernel.Utils.CalculateDistance (distanceBetweenInMeters)
import Kernel.Utils.Common
import qualified SharedLogic.IntegratedBPPConfig as SIBC
import qualified SharedLogic.SharedCab.Allocation as Allocation
import qualified SharedLogic.SharedCab.Allocation.Types as AllocTypes
import qualified SharedLogic.SharedCab.Booking as Booking
import qualified SharedLogic.SharedCab.BookingState as BookingState
import qualified SharedLogic.SharedCab.CabSync as CabSync
import qualified SharedLogic.SharedCab.Call as SharedCabCall
import qualified SharedLogic.SharedCab.Config as Config
import qualified SharedLogic.SharedCab.Demand as Demand
import SharedLogic.SharedCab.DriverAction (DriverAction (..), runDriverAction)
import SharedLogic.SharedCab.Enabled (requireSharedCabEnabled)
import qualified SharedLogic.SharedCab.LegState as LegState
import qualified SharedLogic.SharedCab.Lts as Lts
import qualified SharedLogic.SharedCab.Offer as Offer
import SharedLogic.SharedCab.RateLimit (enforceCallLimit, enforceChatMessageLimit)
import qualified SharedLogic.SharedCab.RouteSnapshot as RouteSnapshot
import SharedLogic.SharedCab.Schedule (ensureAllocationTick, ensureDegradedSweep, ensureExpiryJob)
import qualified SharedLogic.SharedCab.Seats as Seats
import qualified SharedLogic.SharedCab.Session as Session
import SharedLogic.SharedCab.SessionState
import qualified SharedLogic.SharedCab.SessionView as View
import qualified Storage.CachedQueries.IntegratedBPPConfig as CQIBC
import qualified Storage.CachedQueries.OTPRest.OTPRest as OTPRest
import qualified Storage.Queries.FRFSTicket as QFRFSTicket
import qualified Storage.Queries.FRFSTicketBooking as QFRFSTicketBooking
import qualified Storage.Queries.JourneyLeg as QJourneyLeg
import qualified Storage.Queries.PersonExtra as QPersonExtra
import Tools.Error
import Tools.Notifications (SharedCabChatEntityData (..), notifyChatMessage)

checkToken :: Maybe Text -> Environment.Flow ()
checkToken mbToken = do
  internalAPIKey <- asks (.internalAPIKey)
  unless (Just internalAPIKey == mbToken) $
    throwError $ AuthBlocked "Invalid BPP internal api key"

getIntegratedBppConfig :: Id DIBC.IntegratedBPPConfig -> Environment.Flow DIBC.IntegratedBPPConfig
getIntegratedBppConfig ibcId = CQIBC.findById ibcId >>= fromMaybeM IntegratedBPPConfigNotFound

-- | The driver's identifier for its city config. IBC ids are per-DB UUIDs and never mirrored, so the driver app sends
-- integrated_bpp_config.agency_key, the GTFS agency gtfsId of the shared-cab feed ("<feed>:SHARED_CAB").
getIntegratedBppConfigByAgency :: Text -> Environment.Flow DIBC.IntegratedBPPConfig
getIntegratedBppConfigByAgency agencyId =
  SIBC.findSharedCabConfig agencyId DIBC.APPLICATION

routeStops :: DIBC.IntegratedBPPConfig -> Text -> Environment.Flow [DRSM.RouteStopMapping]
routeStops integratedBppConfig code = sortOn (.sequenceNum) <$> OTPRest.getRouteStopMappingByRouteCode code integratedBppConfig

routeDirection :: [DRSM.RouteStopMapping] -> Text
routeDirection = maybe "" (.stopName) . listToMaybe . reverse

-- | The city's version of the feed, kept 5 min: every demand read carries it.
feedVersion :: DIBC.IntegratedBPPConfig -> Environment.Flow (Maybe Text)
feedVersion integratedBppConfig =
  IM.withInMemCache ["SharedCabFeedVersion", integratedBppConfig.feedKey] 300 $
    either (const Nothing) Just <$> withTryCatch "getGtfsVersion:sharedCab" (OTPRest.getGtfsVersion integratedBppConfig)

postSharedCabRoutesDemand :: Maybe Text -> API.SharedCabDemandReq -> Environment.Flow View.SharedCabDemandResp
postSharedCabRoutesDemand mbToken req = do
  checkToken mbToken
  integratedBppConfig <- getIntegratedBppConfigByAgency req.agencyId
  let cityId = integratedBppConfig.merchantOperatingCityId
  cfg <- Config.getTunables cityId
  when (length req.routeCodes > cfg.maxDemandRoutes) $ throwError (InvalidRequest "Too many routes for one demand read")
  candidates <- forM req.routeCodes $ \routeCode -> (routeCode,) . take cfg.demandStopsPerRoute . map (.stopCode) <$> routeStops integratedBppConfig routeCode
  stops <- if null candidates then pure [] else Demand.demandOnRoutes cfg cityId.getId candidates
  publicTransportVersion <- feedVersion integratedBppConfig
  pure View.SharedCabDemandResp {publicTransportVersion, stops}

required :: Text -> Maybe a -> Environment.Flow a
required field = fromMaybeM (InvalidRequest (field <> " is required for this operation"))

-- | Driver-app's run of the cab: every change, and every session poll (which brings the copy back after a flush). An open or a
-- resume needs the city on, and seeds the city's chains; the reply is the driver's view and the riders on board.
postSharedCabCabSync :: Maybe Text -> CabUpdate -> Environment.Flow CabSyncResp
postSharedCabCabSync mbToken update = do
  checkToken mbToken
  let opening = update.change `elem` [Just CAB_OPENED, Just CAB_RESUMED]
  when opening $ getIntegratedBppConfigByAgency update.cab.agencyId >>= requireSharedCabEnabled . (.merchantOperatingCityId)
  s <- CabSync.applyCabUpdate update
  when (opening || isNothing update.change) $ seed s
  onBoard <- Booking.ridersOnBoard s.vehicleNumber >>= mapM CabSync.cabRiderOf
  session <-
    if s.status == ENDED
      then pure Nothing
      else Just <$> mkSessionRespFrom (if isNothing update.change then CachedCab else FreshCab) s
  pure CabSyncResp {session, ridersOnBoard = onBoard}

-- | The city's allocation tick, expiry check and degraded sweep run while a cab is live; each guard makes a re-seed a no-op.
seed :: Session -> Environment.Flow ()
seed s = when (s.status /= ENDED) $ do
  ensureAllocationTick s.merchantId s.merchantOperatingCityId
  ensureExpiryJob s.merchantId s.merchantOperatingCityId
  withTryCatch "sharedCabSeedSweep" (ensureDegradedSweep s.merchantId s.merchantOperatingCityId) >>= \case
    Left e -> logError $ "sharedCab: couldn't seed the degraded sweep for " <> s.vehicleNumber <> ": " <> show e
    Right () -> pure ()

-- | Walk-ups and cab full are seats, decided against the bookings. Cab full: every unboarded allocation goes as SEAT_LOST, then
-- walk-ups take the seats left, both under the plate lock; the releases' pushes and re-ticks go out after it.
postSharedCabSessionOperations :: Maybe Text -> API.SharedCabSessionOperationReq -> Environment.Flow SharedCabSeatsResp
postSharedCabSessionOperations mbToken req = do
  checkToken mbToken
  case req.operation of
    API.SET_SEATS -> do
      version <- required "version" req.version
      walkupCount <- required "walkupCount" req.walkupCount
      (s, walkupsAdded) <- Session.setWalkupCount req.driverId req.vehicleNumber version walkupCount
      session <- mkSessionResp s
      pure SharedCabSeatsResp {session, walkupsAdded}
    API.CAB_FULL -> do
      (full, released) <- Session.markCabFull req.driverId req.vehicleNumber (Allocation.closeUnboarded plate AllocTypes.SeatLost)
      Allocation.afterUnboarded plate AllocTypes.SeatLost released
      session <- mkSessionResp full
      pure SharedCabSeatsResp {session, walkupsAdded = 0}
  where
    plate = req.vehicleNumber

-- | The driver answers the offer on a newly allocated rider. Accepting arms the claim's normal timer and tells the rider; declining closes the
-- allocation (the plate is skipped for this booking) and the next tick offers the next cab.
postSharedCabBookingOperations :: Id DFTB.FRFSTicketBooking -> Maybe Text -> API.SharedCabBookingOperationReq -> Environment.Flow View.SharedCabSession
postSharedCabBookingOperations bookingId mbToken req = do
  checkToken mbToken
  case req.operation of
    API.ACCEPT -> Offer.acceptOffer req.driverId req.vehicleNumber bookingId >>= owned >>= mkSessionResp
    API.DECLINE -> Offer.declineOffer req.driverId req.vehicleNumber bookingId >>= owned >>= mkSessionResp
    API.CANCEL -> driverAction DriverCancel
    API.BOARDED_WITHOUT_CODE -> driverAction DriverBoarded
    API.DROPPED -> driverAction DriverDropped
  where
    driverAction action = runDriverAction action req.driverId req.vehicleNumber req.reason bookingId >>= mkSessionResp
    owned = either throwError pure . ownedSession req.driverId . Just

sessionRoute :: DIBC.IntegratedBPPConfig -> Text -> Environment.Flow View.SessionRoute
sessionRoute integratedBppConfig code = do
  mbRoute <- OTPRest.getRouteByRouteId integratedBppConfig code
  stops <- routeStops integratedBppConfig code
  pure
    View.SessionRoute
      { code,
        name = maybe code (.longName) mbRoute,
        direction = routeDirection stops,
        nextStops = map (.stopName) stops
      }

-- | The plate's live bookings as rider rows; status/ETA/deadline come from the booking's real state.
liveRiderRows :: Config.SharedCabTunables -> Text -> [(Text, LatLong)] -> [LatLong] -> Map.Map Text Text -> Text -> Environment.Flow [View.RiderRow]
liveRiderRows cfg sessionTrip stopPoints polyline addresses plate = do
  bookings <- Booking.liveBookingsForVehicle plate
  if null bookings
    then pure []
    else do
      tickets <- QFRFSTicket.findAllByTicketBookingIds (map (.id) bookings)
      persons <- QPersonExtra.findAllByIds (map (.riderId) bookings)
      now <- getCurrentTime
      let pointsByCode = Map.fromList stopPoints
          nameOf b = fromMaybe "Rider" $ listToMaybe [w | p <- persons, p.id == b.riderId, Just n <- [p.firstName], w <- T.words n]
          statusesOf b = [t.status | t <- tickets, t.frfsTicketBookingId == b.id]
          onThisTrip b = LegState.tripMatches b.vehicleTripId sessionTrip
      parties <- Booking.partySizes bookings
      allocs <- BookingState.readAllocs (map (.id) bookings)
      fixes <- Booking.readRiderFixes (map (.id) bookings)
      forM (zip3 bookings allocs fixes) $ \(b, mbAlloc, mbFix) -> do
        let statuses = statusesOf b
            boarded = DFRFSTicket.INPROGRESS `elem` statuses
        -- the allocation's armed timer (stand/moving); else the key's own TTL is the allocation's outer bound
        expiresAt <- case mbAlloc of
          Just st | isJust (AllocTypes.allocationDeadline st) -> pure (AllocTypes.allocationDeadline st)
          Just _ -> do
            ttl <- BookingState.bookingStateTtl b.id
            pure $ if ttl > 0 then Just (addUTCTime (fromInteger ttl) now) else Nothing
          Nothing -> pure Nothing
        let offerDeadline =
              mbAlloc >>= \st -> case st.timer of
                AllocTypes.OfferTimer d -> Just d
                _ -> Nothing
        let callable = cfg.callEnabled && onThisTrip b && isNothing offerDeadline
        -- decrypting on every 5 s session poll is costly: cached in this pod's memory only, never in Redis or a log
        callNumber <-
          if callable && Config.activeCallMode cfg == Just DRC.DIRECT
            then maybe (pure Nothing) (IM.withInMemCache ["SharedCabRiderNumber", b.riderId.getId] 120 . SharedCabCall.personCallNumber) (find ((== b.riderId) . (.id)) persons)
            else pure Nothing
        let minutesAway = case (mbFix, Map.lookup b.fromStationCode pointsByCode) of
              (Just lastFix, Just stopPoint) -> Just $ View.walkMinutesAway (realToFrac (distanceBetweenInMeters lastFix.position stopPoint))
              _ -> Nothing
        pure
          View.RiderRow
            { bookingId = b.id.getId,
              firstName = nameOf b,
              seats = Booking.bookingSeats (Booking.partyOf parties b) statuses,
              boardStopCode = b.fromStationCode,
              dropStopCode = b.toStationCode,
              boarded,
              fare = b.totalPrice.amount,
              riderStatus = View.riderStatusOf boarded (isJust mbAlloc),
              minutesAway,
              expiresAt,
              -- a live row holds a seat on a live cab; a rider boarded on an earlier run of the plate is not this run's rider
              chatChannelId = if cfg.chatEnabled && onThisTrip b && isNothing offerDeadline then Just b.id.getId else Nothing,
              callable,
              callNumber,
              offerExpiresAt = offerDeadline,
              boardStopAddress = Map.lookup b.fromStationCode addresses,
              dropStopAddress = Map.lookup b.toStationCode addresses,
              distanceMeters = View.rideMeters polyline stopPoints b.fromStationCode b.toStationCode,
              riderRating = View.roundedRating (realToFrac <$> listToMaybe [r | p <- persons, p.id == b.riderId, Just r <- [p.rating]])
            }

-- | Until the tick lands: moving, next stops are the whole route. `available` is the seats free from the cab's next stop
-- to the route's end: what walk-ups could still take.
data CabReads = FreshCab | CachedCab

mkSessionResp :: Session -> Environment.Flow View.SharedCabSession
mkSessionResp = mkSessionRespFrom FreshCab

mkSessionRespFrom :: CabReads -> Session -> Environment.Flow View.SharedCabSession
mkSessionRespFrom cabReads s = do
  integratedBppConfig <- getIntegratedBppConfig s.integratedBppConfigId
  route <- sessionRoute integratedBppConfig s.routeCode
  stops <- routeStops integratedBppConfig s.routeCode
  cfg <- Config.getTunables s.merchantOperatingCityId
  demand <- Demand.demandByStop cfg s.merchantOperatingCityId.getId s.routeCode (map (.stopCode) stops)
  queuedRoute <- traverse (sessionRoute integratedBppConfig) s.queuedRouteCode
  addresses <- Booking.stopAddresses integratedBppConfig.merchantOperatingCityId (map (.stopCode) stops)
  shapePolyline <- (>>= (.polyline)) <$> OTPRest.getRouteByRouteId integratedBppConfig s.routeCode
  let polyline = maybe [] KEPP.decode shapePolyline
  snapshotCab <- case cabReads of
    CachedCab -> find ((== s.vehicleNumber) . (.session.vehicleNumber)) <$> RouteSnapshot.routeSnapshot s.merchantOperatingCityId s.routeCode
    FreshCab -> pure Nothing
  -- a cab missing from the snapshot (PAUSED, or not yet in it) is read fresh
  (mbTrack, available) <- case snapshotCab of
    Just c -> pure (c.track, Seats.freeOver c.seats (Seats.nextStopIdx c.seats ((.vehicleInfo) <$> c.track), Seats.routeEnd c.seats))
    Nothing -> do
      mbTrack <- Lts.readRoutePosition s.routeCode s.vehicleNumber
      (mbTrack,) <$> Seats.freeAhead s ((.vehicleInfo) <$> mbTrack)
  let vehicle = (\v -> LatLong v.vehicleInfo.latitude v.vehicleInfo.longitude) <$> mbTrack
  riderRows <- liveRiderRows cfg s.vehicleTripId [(stop.stopCode, stop.stopPoint) | stop <- stops] polyline addresses s.vehicleNumber
  pure
    View.SharedCabSession
      { route,
        tripId = s.vehicleTripId,
        gtfsId = integratedBppConfig.feedKey,
        queuedRoute,
        status = s.status,
        pauseReason = s.pauseReason,
        movement = SessionMovementMoving,
        capacity = s.capacity,
        walkupCount = s.walkupCount,
        available,
        version = s.version,
        riders = View.ridersOf [(st.stopCode, st.stopName) | st <- stops] riderRows,
        pendingOffers = View.pendingOffersOf [(st.stopCode, st.stopName) | st <- stops] riderRows,
        stops = [View.sessionStopOf st.stopCode st.stopName st.stopPoint | st <- stops],
        shapePolyline,
        vehicle,
        demandAhead = Demand.demandAheadRows [(st.stopCode, st.stopName) | st <- stops] demand
      }

-- | The rider's number for a driver-initiated masked call; only for the driver whose live cab holds the booking, inside the call window.
getSharedCabBookingCallNumber :: Id DFTB.FRFSTicketBooking -> Text -> Text -> Maybe Text -> Environment.Flow API.RiderCallNumberResp
getSharedCabBookingCallNumber bookingId driverId vehicleNumber mbToken = do
  checkToken mbToken
  booking <- QFRFSTicketBooking.findById bookingId >>= fromMaybeM (FRFSTicketBookingDoesNotExist bookingId.getId)
  cfg <- Config.getTunables booking.merchantOperatingCityId
  unless cfg.callEnabled $ throwError SharedCabCallDisabled
  mbSession <- SharedCabCall.liveSession cfg.callEnabled booking
  case mbSession of
    Just s | s.driverId == driverId,
             s.vehicleNumber == vehicleNumber -> do
      enforceCallLimit bookingId driverId
      API.RiderCallNumberResp <$> SharedCabCall.riderCallNumber booking
    _ -> throwError SharedCabCallNotAvailable

-- | A driver's chat message to a rider's cab chat as a push. Same authorisation as the call (the city's chat switch instead), so
-- only the driver whose live cab holds the booking on its current trip can write to the rider.
postSharedCabBookingMessage :: Id DFTB.FRFSTicketBooking -> Maybe Text -> API.SharedCabMessageReq -> Environment.Flow APISuccess
postSharedCabBookingMessage bookingId mbToken req = do
  checkToken mbToken
  booking <- QFRFSTicketBooking.findById bookingId >>= fromMaybeM (FRFSTicketBookingDoesNotExist bookingId.getId)
  cfg <- Config.getTunables booking.merchantOperatingCityId
  unless cfg.chatEnabled $ throwError SharedCabChatDisabled
  mbSession <- SharedCabCall.liveSession cfg.chatEnabled booking
  case mbSession of
    Just s | s.driverId == req.driverId, s.vehicleNumber == req.vehicleNumber -> pure ()
    _ -> throwError SharedCabChatNotAvailable
  message <- either (throwError . InvalidRequest) pure (chatMessageBody req.message)
  enforceChatMessageLimit bookingId req.driverId
  mbLeg <- QJourneyLeg.findByLegSearchId (Just booking.searchId.getId)
  notifyChatMessage booking.riderId message (SharedCabChatEntityData {sharedCabBookingId = bookingId.getId, journeyId = (.journeyId.getId) <$> mbLeg})
  pure Success

maxChatMessageChars :: Int
maxChatMessageChars = 500

-- | A driver's chat message as the push body: stripped, not empty and at most maxChatMessageChars.
chatMessageBody :: Text -> Either Text Text
chatMessageBody raw
  | T.null message = Left "message is empty"
  | T.length message > maxChatMessageChars = Left "message is too long"
  | otherwise = Right message
  where
    message = T.strip raw
