module Domain.Action.UI.SharedCabInternal
  ( getSharedCabRoutesData,
    postSharedCabRoutesDemand,
    postSharedCabRouteOperations,
    postSharedCabSessionOperations,
    postSharedCabBookingOperations,
    getSharedCabSession,
    getSharedCabTrips,
    tripDayWindow,
    onCityDay,
    getSharedCabBookingCallNumber,
    postSharedCabBookingMessage,
    chatMessageBody,
  )
where

import qualified API.Types.UI.SharedCabInternal as API
import qualified Data.Map.Strict as Map
import Data.Maybe (listToMaybe)
import qualified Data.Text as T
import Data.Time (Day, UTCTime (..))
import qualified Domain.Types.FRFSTicketBooking as DFTB
import qualified Domain.Types.FRFSTicketStatus as DFRFSTicket
import qualified Domain.Types.IntegratedBPPConfig as DIBC
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Route as DRoute
import qualified Domain.Types.RouteStopMapping as DRSM
import qualified Domain.Types.VehicleTrip as DVT
import qualified Environment
import EulerHS.Prelude hiding (id)
import qualified Kernel.External.Maps.Google.PolyLinePoints as KEPP
import Kernel.External.Maps.Types (LatLong (..))
import qualified Kernel.Storage.InMem as IM
import Kernel.Types.APISuccess (APISuccess (Success))
import Kernel.Types.Id
import Kernel.Utils.CalculateDistance (distanceBetweenInMeters)
import Kernel.Utils.Common
import Lib.ConfigPilot.Interface.Types (getConfig)
import qualified SharedLogic.SharedCab.Allocation as Allocation
import qualified SharedLogic.SharedCab.Allocation.Types as AllocTypes
import qualified SharedLogic.SharedCab.Booking as Booking
import qualified SharedLogic.SharedCab.BookingState as BookingState
import qualified SharedLogic.SharedCab.Call as SharedCabCall
import qualified SharedLogic.SharedCab.Config as Config
import qualified SharedLogic.SharedCab.Demand as Demand
import SharedLogic.SharedCab.DriverAction (DriverAction (..), runDriverAction)
import qualified SharedLogic.SharedCab.LegState as LegState
import qualified SharedLogic.SharedCab.Lts as Lts
import qualified SharedLogic.SharedCab.Offer as Offer
import SharedLogic.SharedCab.RateLimit (enforceChatMessageLimit)
import qualified SharedLogic.SharedCab.RouteSnapshot as RouteSnapshot
import SharedLogic.SharedCab.Schedule (ensureAllocationTick)
import qualified SharedLogic.SharedCab.Seats as Seats
import qualified SharedLogic.SharedCab.Session as Session
import SharedLogic.SharedCab.SessionState
import qualified SharedLogic.SharedCab.SessionView as View
import qualified Storage.CachedQueries.IntegratedBPPConfig as CQIBC
import qualified Storage.CachedQueries.OTPRest.OTPRest as OTPRest
import qualified Storage.CachedQueries.StationsExtraInformation as CQStationsExtraInformation
import Storage.ConfigPilot.Config.RiderConfig (RiderConfigDimensions (..))
import qualified Storage.Queries.FRFSTicket as QFRFSTicket
import qualified Storage.Queries.FRFSTicketBooking as QFRFSTicketBooking
import qualified Storage.Queries.IntegratedBPPConfigExtra as QIBCExtra
import qualified Storage.Queries.JourneyLeg as QJourneyLeg
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.PersonExtra as QPersonExtra
import qualified Storage.Queries.VehicleTrip as QVT
import Tools.Error
import Tools.Notifications (SharedCabChatEntityData (..), notifyChatMessage)

defaultCapacity :: Int
defaultCapacity = 6

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
  CQIBC.findByAgencyId agencyId DIBC.APPLICATION
    >>= fromMaybeM (IntegratedBPPConfigNotFoundForAgency agencyId (show DIBC.APPLICATION)) . QIBCExtra.onlyOfPlatform DIBC.APPLICATION (.platformType)

routeStops :: DIBC.IntegratedBPPConfig -> Text -> Environment.Flow [DRSM.RouteStopMapping]
routeStops integratedBppConfig code = sortOn (.sequenceNum) <$> OTPRest.getRouteStopMappingByRouteCode code integratedBppConfig

feedRoutes :: DIBC.IntegratedBPPConfig -> Environment.Flow [(DRoute.Route, [DRSM.RouteStopMapping])]
feedRoutes integratedBppConfig = do
  routes <- OTPRest.getRoutesByGtfsIdWithPolyline integratedBppConfig
  forM routes $ \route -> (route,) <$> routeStops integratedBppConfig route.code

routeDirection :: [DRSM.RouteStopMapping] -> Text
routeDirection = maybe "" (.stopName) . listToMaybe . reverse

-- | The city's version of the feed, kept 5 min: every demand read carries it.
feedVersion :: DIBC.IntegratedBPPConfig -> Environment.Flow (Maybe Text)
feedVersion integratedBppConfig =
  IM.withInMemCache ["SharedCabFeedVersion", integratedBppConfig.feedKey] 300 $
    either (const Nothing) Just <$> withTryCatch "getGtfsVersion:sharedCab" (OTPRest.getGtfsVersion integratedBppConfig)

getSharedCabRoutesData :: Text -> Maybe Text -> Environment.Flow View.SharedCabRouteDataResp
getSharedCabRoutesData agencyId mbToken = do
  checkToken mbToken
  integratedBppConfig <- getIntegratedBppConfigByAgency agencyId
  routes <- feedRoutes integratedBppConfig
  publicTransportVersion <- feedVersion integratedBppConfig
  let toResp (route, stops) =
        View.SharedCabRoute
          { code = route.code,
            name = route.longName,
            direction = routeDirection stops,
            fromStop = maybe "" (.stopName) (listToMaybe stops),
            toStop = routeDirection stops,
            stops = [View.sessionStopOf s.stopCode s.stopName s.stopPoint | s <- stops],
            shapePolyline = route.polyline
          }
  pure View.SharedCabRouteDataResp {publicTransportVersion, routes = map toResp routes}

maxDemandRoutes :: Int
maxDemandRoutes = 50

postSharedCabRoutesDemand :: Maybe Text -> API.SharedCabDemandReq -> Environment.Flow View.SharedCabDemandResp
postSharedCabRoutesDemand mbToken req = do
  checkToken mbToken
  when (length req.routeCodes > maxDemandRoutes) $ throwError (InvalidRequest "Too many routes for one demand read")
  integratedBppConfig <- getIntegratedBppConfigByAgency req.agencyId
  let cityId = integratedBppConfig.merchantOperatingCityId
  cfg <- Config.getTunables cityId
  candidates <- forM req.routeCodes $ \routeCode -> (routeCode,) . take cfg.demandStopsPerRoute . map (.stopCode) <$> routeStops integratedBppConfig routeCode
  stops <- Demand.demandOnRoutes cfg cityId.getId candidates
  publicTransportVersion <- feedVersion integratedBppConfig
  pure View.SharedCabDemandResp {publicTransportVersion, stops}

required :: Text -> Maybe a -> Environment.Flow a
required field = fromMaybeM (InvalidRequest (field <> " is required for this operation"))

postSharedCabRouteOperations :: Maybe Text -> API.SharedCabRouteOperationReq -> Environment.Flow API.SelectRouteResp
postSharedCabRouteOperations mbToken req = do
  checkToken mbToken
  case req.operation of
    API.SELECT_ROUTE -> selectRoute req
    API.RETURN_TRIP -> ended . Just =<< (Session.endRoute req.driverId req.vehicleNumber forced StartReturn >>= releasing AllocTypes.RouteChanged >>= mkSessionResp)
    API.CHANGE_ROUTE -> ended . Just =<< (ownSession req.driverId req.vehicleNumber >>= mkSessionResp)
    API.END_ROUTE -> do
      void $ Session.endRoute req.driverId req.vehicleNumber forced (if req.atLastStop == Just True then EndRoute else EndForNow) >>= releasing AllocTypes.SessionClosed
      ended Nothing
  where
    forced = req.forceEnd == Just True
    ended session = pure API.SelectRouteResp {session, affectedRiders = Nothing}

selectRoute :: API.SharedCabRouteOperationReq -> Environment.Flow API.SelectRouteResp
selectRoute req = do
  agencyId <- required "agencyId" req.agencyId
  serviceTierType <- required "serviceTierType" req.serviceTierType
  routeCode <- required "routeCode" req.routeCode
  walkupCount <- required "walkupCount" req.walkupCount
  integratedBppConfig <- getIntegratedBppConfigByAgency agencyId
  priorRoute <- fmap (.routeCode) . mfilter ((/= ENDED) . (.status)) <$> Session.getSessionFresh req.vehicleNumber
  selected <-
    Session.selectRoute
      req.mode
      OpenSessionReq
        { driverId = req.driverId,
          vehicleNumber = req.vehicleNumber,
          merchantId = integratedBppConfig.merchantId,
          merchantOperatingCityId = integratedBppConfig.merchantOperatingCityId,
          integratedBppConfigId = integratedBppConfig.id,
          serviceTierType = serviceTierType,
          capacity = fromMaybe defaultCapacity req.capacity,
          routeCode = routeCode
        }
  case selected of
    Left onBoard -> do
      affected <- mapM affectedRider onBoard
      pure API.SelectRouteResp {session = Nothing, affectedRiders = Just affected}
    Right session -> do
      void $ seeded session
      Session.setCabDriverInfo req.vehicleNumber Session.CabDriverInfo {driverName = req.driverName, vehicleModel = req.vehicleModel}
      when (maybe False (/= session.routeCode) priorRoute) $ void $ releasing AllocTypes.RouteChanged session
      session' <-
        if walkupCount == session.walkupCount
          then pure session
          else Session.setWalkupCount req.driverId req.vehicleNumber session.version walkupCount
      resp <- mkSessionResp session'
      pure API.SelectRouteResp {session = Just resp, affectedRiders = Nothing}
  where
    affectedRider (booking :: DFTB.FRFSTicketBooking) = do
      mbRider <- QPerson.findById booking.riderId
      pure
        API.AffectedRider
          { bookingId = booking.id.getId,
            firstName = fromMaybe "" (mbRider >>= (.firstName)),
            dropStop = fromMaybe booking.toStationCode booking.toStationName
          }

seeded :: Session -> Environment.Flow Session
seeded s = s <$ ensureAllocationTick s.merchantId s.merchantOperatingCityId

releasing :: AllocTypes.AllocationOutcome -> Session -> Environment.Flow Session
releasing outcome s = s <$ Allocation.releaseUnboarded s.vehicleNumber outcome

ownSession :: Text -> Text -> Environment.Flow Session
ownSession driver plate = Session.getSession plate >>= either throwError pure . ownedSession driver

-- | The driver's session poll reads the cab's fix and seats from the route snapshot; an action's reply reads them fresh.
getSharedCabSession :: Text -> Text -> Maybe Text -> Environment.Flow View.SharedCabSession
getSharedCabSession driver plate mbToken = checkToken mbToken >> ownSession driver plate >>= mkSessionRespFrom CachedCab

-- | Cab full: every unboarded allocation goes as SEAT_LOST, then walk-ups take the seats left, both under the plate lock; the releases'
-- pushes and re-ticks go out after it.
postSharedCabSessionOperations :: Maybe Text -> API.SharedCabSessionOperationReq -> Environment.Flow View.SharedCabSession
postSharedCabSessionOperations mbToken req = do
  checkToken mbToken
  case req.operation of
    API.SET_SEATS -> do
      version <- required "version" req.version
      walkupCount <- required "walkupCount" req.walkupCount
      Session.setWalkupCount req.driverId req.vehicleNumber version walkupCount >>= mkSessionResp
    API.RESUME -> Session.resume req.driverId req.vehicleNumber >>= seeded >>= mkSessionResp
    API.CAB_FULL -> do
      (full, released) <- Session.markCabFull req.driverId req.vehicleNumber (Allocation.closeUnboarded plate AllocTypes.SeatLost)
      Allocation.afterUnboarded plate AllocTypes.SeatLost released
      mkSessionResp full
  where
    plate = req.vehicleNumber

-- | The driver answers the offer on a newly allocated rider. Accepting arms the claim's normal timer and tells the rider; declining closes the
-- allocation (the plate is skipped for this booking) and the next tick offers the next cab.
postSharedCabBookingOperations :: Id DFTB.FRFSTicketBooking -> Maybe Text -> API.SharedCabBookingOperationReq -> Environment.Flow View.SharedCabSession
postSharedCabBookingOperations bookingId mbToken req = do
  checkToken mbToken
  case req.operation of
    API.ACCEPT -> Offer.acceptOffer req.driverId req.vehicleNumber bookingId >> getSharedCabSession' req.driverId req.vehicleNumber
    API.DECLINE -> Offer.declineOffer req.driverId req.vehicleNumber bookingId >> getSharedCabSession' req.driverId req.vehicleNumber
    API.CANCEL -> driverAction DriverCancel
    API.BOARDED_WITHOUT_CODE -> driverAction DriverBoarded
    API.DROPPED -> driverAction DriverDropped
  where
    driverAction action = runDriverAction action req.driverId req.vehicleNumber req.reason bookingId >>= mkSessionResp

getSharedCabSession' :: Text -> Text -> Environment.Flow View.SharedCabSession
getSharedCabSession' driver plate = ownSession driver plate >>= mkSessionResp

getSharedCabTrips :: Day -> Text -> Maybe Text -> Environment.Flow API.SharedCabTripsResp
getSharedCabTrips date driver mbToken = do
  checkToken mbToken
  let (from, to) = tripDayWindow date
  candidates <- QVT.findAllByDriverIdAndStartedAtRange Nothing Nothing driver from to
  trips <- filterM (\trip -> (\offset -> onCityDay date offset trip.startedAt) <$> cityUtcOffset trip.merchantOperatingCityId) candidates
  pure API.SharedCabTripsResp {trips = map mkTrip trips}
  where
    cityUtcOffset cityId = maybe (Seconds 19800) (.timeDiffFromUtc) <$> getConfig (RiderConfigDimensions {merchantOperatingCityId = cityId.getId}) Nothing
    mkTrip (trip :: DVT.VehicleTrip) =
      API.SharedCabTrip
        { id = trip.id,
          routeCode = trip.routeCode,
          status = trip.status,
          startedAt = trip.startedAt,
          endedAt = trip.endedAt,
          endReason = trip.endReason,
          offlineBoardings = trip.offlineBoardings
        }

-- | A trip's calendar day is its city's local day. Every UTC offset is under a day, so the day lies inside the UTC day either side.
tripDayWindow :: Day -> (UTCTime, UTCTime)
tripDayWindow date = (addUTCTime (-86400) (UTCTime date 0), addUTCTime (2 * 86400 - 1) (UTCTime date 0))

onCityDay :: Day -> Seconds -> UTCTime -> Bool
onCityDay date utcOffset startedAt = utctDay (addUTCTime (secondsToNominalDiffTime utcOffset) startedAt) == date

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
          onThisTrip b = LegState.tripMatches (getId <$> b.vehicleTripId) sessionTrip
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
              callable = cfg.callEnabled && onThisTrip b && isNothing offerDeadline,
              offerExpiresAt = offerDeadline,
              boardStopAddress = Map.lookup b.fromStationCode addresses,
              dropStopAddress = Map.lookup b.toStationCode addresses,
              distanceMeters = View.rideMeters polyline stopPoints b.fromStationCode b.toStationCode,
              riderRating = View.roundedRating (realToFrac <$> listToMaybe [r | p <- persons, p.id == b.riderId, Just r <- [p.rating]])
            }

-- | The stops' addresses from the city's station information (cached); a stop without one is left out.
stopAddresses :: Id DMOC.MerchantOperatingCity -> [Text] -> Environment.Flow (Map.Map Text Text)
stopAddresses cityId codes =
  Map.fromList . mapMaybe (\info -> (info.stationId,) <$> info.address) <$> CQStationsExtraInformation.getBystationIdsAndCity codes cityId

-- | Until the tick lands: moving, next stops are the whole route. `available` is what the seat engine has free from the cab's next stop
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
  addresses <- stopAddresses integratedBppConfig.merchantOperatingCityId (map (.stopCode) stops)
  shapePolyline <- (>>= (.polyline)) <$> OTPRest.getRouteByRouteId integratedBppConfig s.routeCode
  let polyline = maybe [] KEPP.decode shapePolyline
  snapshotCab <- case cabReads of
    CachedCab -> find ((== s.vehicleNumber) . (.session.vehicleNumber)) <$> RouteSnapshot.routeSnapshot s.routeCode
    FreshCab -> pure Nothing
  -- a cab missing from the snapshot (PAUSED, or not yet in it) is read fresh
  (mbTrack, available) <- case snapshotCab of
    Just c -> do
      seats <- Seats.cabSeats s
      pure (c.track, Seats.freeOverBitmaps (RouteSnapshot.cabBitmapsOf c) (Seats.nextStopIdx seats ((.vehicleInfo) <$> c.track), Seats.routeEnd seats))
    Nothing -> do
      mbTrack <- Lts.readRoutePosition s.routeCode s.vehicleNumber
      (mbTrack,) <$> Seats.freeAhead s ((.vehicleInfo) <$> mbTrack)
  let vehicle = (\v -> LatLong v.vehicleInfo.latitude v.vehicleInfo.longitude) <$> mbTrack
  riders <- liveRiderRows cfg s.vehicleTripId.getId [(stop.stopCode, stop.stopPoint) | stop <- stops] polyline addresses s.vehicleNumber
  pure
    View.SharedCabSession
      { route,
        queuedRoute,
        status = s.status,
        pauseReason = s.pauseReason,
        movement = SessionMovementMoving,
        capacity = s.capacity,
        walkupCount = s.walkupCount,
        available,
        version = s.version,
        ridersByStop = View.groupRidersByStop [(st.stopCode, st.stopName) | st <- stops] riders,
        pendingOffers = View.pendingOffersOf [(st.stopCode, st.stopName) | st <- stops] riders,
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
    Just s | s.driverId == driverId, s.vehicleNumber == vehicleNumber -> API.RiderCallNumberResp <$> SharedCabCall.riderCallNumber booking
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
