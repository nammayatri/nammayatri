module Domain.Action.UI.SharedCabInternal
  ( getSharedCabRoutes,
    postSharedCabRouteSelect,
    getSharedCabSession,
    postSharedCabSeats,
    postSharedCabRouteEnd,
    postSharedCabResume,
    getSharedCabTrips,
  )
where

import qualified API.Types.UI.SharedCabInternal as API
import Data.List (sortOn)
import Data.Maybe (listToMaybe)
import Data.Time (Day, UTCTime (..), addUTCTime)
import qualified Domain.Types.FRFSTicketBooking as DFTB
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
import qualified SharedLogic.SharedCab.Session as Session
import SharedLogic.SharedCab.SessionState
import qualified SharedLogic.SharedCab.SessionView as View
import qualified Storage.CachedQueries.IntegratedBPPConfig as CQIBC
import qualified Storage.CachedQueries.OTPRest.OTPRest as OTPRest
import qualified Storage.Queries.Person as QPerson
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
  selected <-
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
      session' <-
        if req.walkupCount == session.walkupCount
          then pure session
          else Session.setWalkupCount req.driverId req.vehicleNumber session.version req.walkupCount
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

ownSession :: Text -> Text -> Environment.Flow Session
ownSession driver plate = Session.getSession plate >>= either throwError pure . ownedSession driver

getSharedCabSession :: Text -> Text -> Maybe Text -> Environment.Flow View.SharedCabSession
getSharedCabSession driver plate mbToken = do
  checkToken mbToken
  ownSession driver plate >>= mkSessionResp

postSharedCabSeats :: Maybe Text -> API.SeatsReq -> Environment.Flow View.SharedCabSession
postSharedCabSeats mbToken req = do
  checkToken mbToken
  Session.setWalkupCount req.driverId req.vehicleNumber req.version req.walkupCount >>= mkSessionResp

-- | CHANGE leaves the session as is: the driver picks the next route with route/select, which closes this run.
postSharedCabRouteEnd :: Maybe Text -> View.EndRouteReq -> Environment.Flow (Maybe View.SharedCabSession)
postSharedCabRouteEnd mbToken req = do
  checkToken mbToken
  case req.next of
    View.RETURN -> Just <$> (Session.endRoute req.driverId req.vehicleNumber forced StartReturn >>= mkSessionResp)
    View.CHANGE -> Just <$> (ownSession req.driverId req.vehicleNumber >>= mkSessionResp)
    View.END -> Nothing <$ Session.endRoute req.driverId req.vehicleNumber forced (if req.atLastStop == Just True then EndRoute else EndForNow)
  where
    forced = req.force == Just True

postSharedCabResume :: Maybe Text -> API.SharedCabDriverReq -> Environment.Flow View.SharedCabSession
postSharedCabResume mbToken req = do
  checkToken mbToken
  Session.resume req.driverId req.vehicleNumber >>= mkSessionResp

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

-- | Until the tick and allocation land: movement is MOVING, next stops are the whole route,
-- riders/demand are empty and `available` counts walk-ups only.
mkSessionResp :: Session -> Environment.Flow View.SharedCabSession
mkSessionResp s = do
  integratedBppConfig <- getIntegratedBppConfig s.integratedBppConfigId
  route <- sessionRoute integratedBppConfig s.routeCode
  queuedRoute <- traverse (sessionRoute integratedBppConfig) s.queuedRouteCode
  pure
    View.SharedCabSession
      { route,
        queuedRoute,
        status = s.status,
        pauseReason = s.pauseReason,
        movement = MOVING,
        capacity = s.capacity,
        walkupCount = s.walkupCount,
        available = s.capacity - s.walkupCount,
        version = s.version,
        ridersByStop = [],
        demandAhead = [],
        lowDemandCard = Nothing,
        offRoute = Nothing
      }
