module Domain.Action.UI.SharedCabInternal
  ( getSharedCabRoutes,
    postSharedCabRouteSelect,
    getSharedCabSession,
    postSharedCabSeats,
    postSharedCabRouteEnd,
    postSharedCabResume,
    getSharedCabTrips,
    postSharedCabBookingCancel,
    postSharedCabBookingBoardedWithoutCode,
    postSharedCabBookingDropped,
    postSharedCabCabFull,
  )
where

import qualified API.Types.UI.SharedCabInternal as API
import qualified Data.Map.Strict as Map
import Data.Maybe (listToMaybe)
import Data.Time (Day, UTCTime (..))
import qualified Domain.Types.FRFSTicketBooking as DFTB
import qualified Domain.Types.FRFSTicketStatus as DFRFSTicket
import qualified Domain.Types.IntegratedBPPConfig as DIBC
import qualified Domain.Types.Route as DRoute
import qualified Domain.Types.RouteStopMapping as DRSM
import qualified Domain.Types.VehicleTrip as DVT
import qualified Environment
import EulerHS.Prelude hiding (id)
import Kernel.External.Maps.Types (LatLong (..))
import Kernel.Types.Id
import Kernel.Utils.CalculateDistance (distanceBetweenInMeters)
import Kernel.Utils.Common
import qualified SharedLogic.SharedCab.Allocation as Allocation
import qualified SharedLogic.SharedCab.Allocation.Types as AllocTypes
import SharedLogic.SharedCab.AllocationSchedule (ensureAllocationTick)
import qualified SharedLogic.SharedCab.Booking as Booking
import qualified SharedLogic.SharedCab.Demand as Demand
import SharedLogic.SharedCab.DriverAction (DriverAction (..), runDriverAction)
import qualified SharedLogic.SharedCab.Invariants as Invariants
import SharedLogic.SharedCab.LegState (seatsHeld)
import SharedLogic.SharedCab.Plate (canonicalisePlate)
import qualified SharedLogic.SharedCab.Session as Session
import SharedLogic.SharedCab.SessionState
import qualified SharedLogic.SharedCab.SessionView as View
import qualified Storage.CachedQueries.IntegratedBPPConfig as CQIBC
import qualified Storage.CachedQueries.OTPRest.OTPRest as OTPRest
import qualified Storage.Queries.FRFSTicket as QFRFSTicket
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.PersonExtra as QPersonExtra
import qualified Storage.Queries.VehicleTrip as QVT
import Tools.Error

-- rider_config.defaultCapacity default (Alto); read from config once that field exists.
defaultCapacity :: Int
defaultCapacity = 4

checkToken :: Maybe Text -> Environment.Flow ()
checkToken mbToken = do
  internalAPIKey <- asks (.internalAPIKey)
  unless (Just internalAPIKey == mbToken) $
    throwError $ AuthBlocked "Invalid BPP internal api key"

getIntegratedBppConfig :: Id DIBC.IntegratedBPPConfig -> Environment.Flow DIBC.IntegratedBPPConfig
getIntegratedBppConfig ibcId = CQIBC.findById ibcId >>= fromMaybeM IntegratedBPPConfigNotFound

routeStops :: DIBC.IntegratedBPPConfig -> Text -> Environment.Flow [DRSM.RouteStopMapping]
routeStops integratedBppConfig code = sortOn (.sequenceNum) <$> OTPRest.getRouteStopMappingByRouteCode code integratedBppConfig

feedRoutes :: DIBC.IntegratedBPPConfig -> Environment.Flow [(DRoute.Route, [DRSM.RouteStopMapping])]
feedRoutes integratedBppConfig = do
  routes <- OTPRest.getRoutesByGtfsId integratedBppConfig
  forM routes $ \route -> (route,) <$> routeStops integratedBppConfig route.code

-- | Routes are one per direction, so a route's direction is where it ends.
routeDirection :: [DRSM.RouteStopMapping] -> Text
routeDirection = maybe "" (.stopName) . listToMaybe . reverse

-- | Every route of the feed, nearest stop first; demand ranking and stand pinning join later.
getSharedCabRoutes :: Text -> Double -> Double -> Maybe Text -> Environment.Flow API.SharedCabRoutesResp
getSharedCabRoutes ibcId driverLat driverLon mbToken = do
  checkToken mbToken
  routes <- getIntegratedBppConfig (Id ibcId) >>= feedRoutes
  let distanceFrom = distanceBetweenInMeters (LatLong driverLat driverLon)
      distanceKm route stops =
        realToFrac . (/ 1000) . foldl' min (distanceFrom route.startPoint) $ map distanceFrom (route.endPoint : map (.stopPoint) stops)
      toResp (route, stops) =
        View.SharedCabRoute
          { code = route.code,
            name = route.longName,
            direction = routeDirection stops,
            fromStop = maybe "" (.stopName) (listToMaybe stops),
            toStop = routeDirection stops,
            distanceKm = Just (distanceKm route stops),
            isStandRoute = False
          }
  pure API.SharedCabRoutesResp {routes = sortOn (.distanceKm) (map toResp routes)}

-- | A route change with riders on board and no `mode` applies nothing and returns them as `affectedRiders`.
postSharedCabRouteSelect :: Maybe Text -> API.SelectRouteReq -> Environment.Flow API.SelectRouteResp
postSharedCabRouteSelect mbToken req = do
  checkToken mbToken
  integratedBppConfig <- getIntegratedBppConfig req.integratedBppConfigId
  priorRoute <- fmap (.routeCode) . mfilter ((/= ENDED) . (.status)) <$> Session.getSession req.vehicleNumber
  selected <-
    checkedCab req.vehicleNumber $
      Session.selectRoute
        req.mode
        OpenSessionReq
          { driverId = req.driverId,
            vehicleNumber = req.vehicleNumber,
            merchantId = integratedBppConfig.merchantId,
            merchantOperatingCityId = integratedBppConfig.merchantOperatingCityId,
            integratedBppConfigId = integratedBppConfig.id,
            serviceTierType = req.serviceTierType,
            capacity = fromMaybe defaultCapacity req.capacity,
            routeCode = req.routeCode
          }
  case selected of
    Left onBoard -> do
      affected <- mapM affectedRider onBoard
      pure API.SelectRouteResp {session = Nothing, affectedRiders = Just affected}
    Right session -> do
      void $ seeded session
      -- 05 §8.7: a route change leaves the unboarded riders of the old route behind (a queued change hasn't happened yet)
      when (maybe False (/= session.routeCode) priorRoute) $ void $ releasing AllocTypes.RouteChanged session
      session' <-
        if req.walkupCount == session.walkupCount
          then pure session
          else Session.setWalkupCount req.driverId req.vehicleNumber session.version req.walkupCount >>= checked
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

-- | Validator layer 4 after every session transition; it logs and counts, never throws.
checked :: Session -> Environment.Flow Session
checked s = s <$ Invariants.checkCab s.vehicleNumber

-- | An ACTIVE session is what a FINDING booking needs, so its city gets an allocation tick chain.
seeded :: Session -> Environment.Flow Session
seeded s = s <$ ensureAllocationTick s.merchantId s.merchantOperatingCityId

-- | 05 §8.7: allocations nobody boarded yet leave with the route; after the session write, outside its lock.
releasing :: AllocTypes.AllocationOutcome -> Session -> Environment.Flow Session
releasing outcome s = s <$ Allocation.releaseUnboarded s.vehicleNumber outcome

checkedCab :: Text -> Environment.Flow a -> Environment.Flow a
checkedCab rawPlate action = action <* Invariants.checkCab (canonicalisePlate rawPlate)

ownSession :: Text -> Text -> Environment.Flow Session
ownSession driver plate = Session.getSession plate >>= either throwError pure . ownedSession driver

getSharedCabSession :: Text -> Text -> Maybe Text -> Environment.Flow View.SharedCabSession
getSharedCabSession driver plate mbToken = do
  checkToken mbToken
  ownSession driver plate >>= mkSessionResp

postSharedCabSeats :: Maybe Text -> API.SeatsReq -> Environment.Flow View.SharedCabSession
postSharedCabSeats mbToken req = do
  checkToken mbToken
  Session.setWalkupCount req.driverId req.vehicleNumber req.version req.walkupCount >>= checked >>= mkSessionResp

-- | CHANGE leaves the session as is: the driver picks the next route with route/select, which closes this run.
postSharedCabRouteEnd :: Maybe Text -> View.EndRouteReq -> Environment.Flow (Maybe View.SharedCabSession)
postSharedCabRouteEnd mbToken req = do
  checkToken mbToken
  case req.next of
    View.RETURN -> Just <$> (Session.endRoute req.driverId req.vehicleNumber forced StartReturn >>= releasing AllocTypes.RouteChanged >>= checked >>= mkSessionResp)
    View.CHANGE -> Just <$> (ownSession req.driverId req.vehicleNumber >>= mkSessionResp)
    View.END -> Nothing <$ (Session.endRoute req.driverId req.vehicleNumber forced (if req.atLastStop == Just True then EndRoute else EndForNow) >>= releasing AllocTypes.SessionClosed >>= checked)
  where
    forced = req.force == Just True

postSharedCabResume :: Maybe Text -> API.SharedCabDriverReq -> Environment.Flow View.SharedCabSession
postSharedCabResume mbToken req = do
  checkToken mbToken
  Session.resume req.driverId req.vehicleNumber >>= seeded >>= checked >>= mkSessionResp

postSharedCabBookingCancel :: Id DFTB.FRFSTicketBooking -> Maybe Text -> API.SharedCabDriverReq -> Environment.Flow View.SharedCabSession
postSharedCabBookingCancel = driverAction DriverCancel

postSharedCabBookingBoardedWithoutCode :: Id DFTB.FRFSTicketBooking -> Maybe Text -> API.SharedCabDriverReq -> Environment.Flow View.SharedCabSession
postSharedCabBookingBoardedWithoutCode = driverAction DriverBoarded

postSharedCabBookingDropped :: Id DFTB.FRFSTicketBooking -> Maybe Text -> API.SharedCabDriverReq -> Environment.Flow View.SharedCabSession
postSharedCabBookingDropped = driverAction DriverDropped

-- | R19: the cab is full. Walk-ups are set first (Session.markCabFull), then every unboarded allocation goes as
-- SEAT_LOST, outside the plate lock since the release takes each booking lock.
postSharedCabCabFull :: Maybe Text -> API.SharedCabDriverReq -> Environment.Flow View.SharedCabSession
postSharedCabCabFull mbToken req = do
  checkToken mbToken
  Session.markCabFull req.driverId req.vehicleNumber >>= releasing AllocTypes.SeatLost >>= checked >>= mkSessionResp

driverAction :: DriverAction -> Id DFTB.FRFSTicketBooking -> Maybe Text -> API.SharedCabDriverReq -> Environment.Flow View.SharedCabSession
driverAction action bookingId mbToken req = do
  checkToken mbToken
  s <- runDriverAction action req.driverId req.vehicleNumber bookingId >>= checked
  Invariants.checkBooking bookingId
  mkSessionResp s

-- | The driver's runs that started on `date` (IST). App riders and cash per run join here once boarding sets
-- frfs_ticket_booking.vehicleTripId.
getSharedCabTrips :: Day -> Text -> Maybe Text -> Environment.Flow API.SharedCabTripsResp
getSharedCabTrips date driver mbToken = do
  checkToken mbToken
  let istMidnight = addUTCTime (-19800) (UTCTime date 0)
  trips <- QVT.findAllByDriverIdAndStartedAtRange Nothing Nothing driver istMidnight (addUTCTime 86399 istMidnight)
  pure API.SharedCabTripsResp {trips = map mkTrip trips}
  where
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

-- | The plate's live bookings as rider rows: seats from the tickets still held, first names in one person query.
liveRiderRows :: Text -> Environment.Flow [View.RiderRow]
liveRiderRows plate = do
  bookings <- Booking.liveBookingsForVehicle plate
  if null bookings
    then pure []
    else do
      tickets <- QFRFSTicket.findAllByTicketBookingIds (map (.id) bookings)
      persons <- QPersonExtra.findAllByIds (map (.riderId) bookings)
      let nameOf b = fromMaybe "" $ listToMaybe [n | p <- persons, p.id == b.riderId, Just n <- [p.firstName]]
          statusesOf b = [t.status | t <- tickets, t.frfsTicketBookingId == b.id]
      pure
        [ View.RiderRow
            { bookingId = b.id.getId,
              firstName = nameOf b,
              seats = seatsHeld (statusesOf b),
              boardStopCode = b.fromStationCode,
              dropStopCode = b.toStationCode,
              boarded = DFRFSTicket.INPROGRESS `elem` statusesOf b,
              fare = b.totalPrice.amount
            }
          | b <- bookings
        ]

-- | Until the tick lands: movement is MOVING, next stops (and so demand ahead) are the whole route. `available` is derived, never counted down: capacity less walk-ups less seats live bookings hold (bookings
-- re-attach by plate after a Redis flush, 04 §3; walk-ups restart at 0 and the driver re-taps them).
mkSessionResp :: Session -> Environment.Flow View.SharedCabSession
mkSessionResp s = do
  integratedBppConfig <- getIntegratedBppConfig s.integratedBppConfigId
  route <- sessionRoute integratedBppConfig s.routeCode
  stops <- routeStops integratedBppConfig s.routeCode
  demand <- Demand.demandByStop s.merchantOperatingCityId.getId s.routeCode (map (.stopCode) stops)
  queuedRoute <- traverse (sessionRoute integratedBppConfig) s.queuedRouteCode
  bookedSeats <- Booking.liveSeatsOnVehicle s.vehicleNumber
  riders <- liveRiderRows s.vehicleNumber
  pure
    View.SharedCabSession
      { route,
        queuedRoute,
        status = s.status,
        pauseReason = s.pauseReason,
        movement = MOVING,
        capacity = s.capacity,
        walkupCount = s.walkupCount,
        available = max 0 (s.capacity - s.walkupCount - bookedSeats),
        version = s.version,
        ridersByStop = View.ridersByStop [(st.stopCode, st.stopName) | st <- stops] riders,
        demandAhead =
          [ View.DemandAtStop {stopName = stop.stopName, waiting = d.waiting, searching = d.searching, windowMin = Demand.searchWindowMin}
            | stop <- stops,
              Just d <- [Map.lookup stop.stopCode demand],
              d.waiting + d.searching > 0
          ],
        lowDemandCard = Nothing,
        offRoute = Nothing
      }
