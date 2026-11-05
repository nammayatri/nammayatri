{-
 Copyright 2022-23, Juspay India Pvt Ltd
 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- | Shared-cab driver-facing actions, only for a SHARED_CAB vehicle. The cab's run (session, vehicle_trip, LTS attach) and the route
-- feed are driver-app's own; demand, seats and booking actions go to rider-app, which owns the bookings and checks under its plate
-- lock that the booking is on this driver's cab. Taxi dispatch never sees these drivers: no taxi service tier allows SHARED_CAB.
module Domain.Action.UI.SharedCab
  ( getPublicTransportData,
    postSharedCabRoutesDemand,
    postSharedCabRouteOperations,
    getSharedCabSession,
    postSharedCabSessionOperations,
    postSharedCabBookingOperations,
    postSharedCabBookingCall,
    postSharedCabBookingMessage,
    getSharedCabCallStatus,
    sharedCabFeedVersion,
    BookingAction (..),
    actionReason,
    bookingActionOf,
  )
where

import API.Types.UI.SharedCab
import Data.List (sortOn)
import qualified Domain.Action.UI.Call as DCall
import qualified Domain.Types.CallStatus as SCS
import qualified Domain.Types.Common as DCommon
import Domain.Types.IntegratedBPPConfig (PlatformType (..))
import qualified Domain.Types.IntegratedBPPConfig as DIBC
import Domain.Types.Merchant
import Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person as SP
import Domain.Types.SharedCabSession
import qualified Domain.Types.Vehicle as DVehicle
import qualified Domain.Types.VehicleTrip as DVT
import qualified Domain.Types.VehicleVariant as DV
import Environment
import EulerHS.Prelude hiding (id)
import Kernel.External.Encryption (decrypt)
import qualified Kernel.Storage.InMem as IM
import Kernel.Types.APISuccess (APISuccess)
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.GtfsDataServer.Domain.Types.RouteStopMapping as RSM
import SharedLogic.CallBAPInternal (AppBackendBapInternal)
import qualified SharedLogic.DriverPool as DP
import SharedLogic.IntegratedBPPConfig (findIntegratedBPPConfig)
import qualified SharedLogic.SharedCab.RiderClient as SharedCabBAP
import qualified SharedLogic.SharedCab.Session as Session
import qualified Storage.CachedQueries.OTPRest.OTPRest as OTPRest
import qualified Storage.Queries.CallStatus as QCallStatus
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.Vehicle as QVehicle
import qualified Tools.Call as TCall
import Tools.Error

type DriverAuthInfo = (Id SP.Person, Id Merchant, Id MerchantOperatingCity)

type TokenAuthInfo = (Maybe (Id SP.Person), Id Merchant, Id MerchantOperatingCity)

getPublicTransportData :: TokenAuthInfo -> Flow SharedCabRouteDataResp
getPublicTransportData auth = requireDriver auth >>= routeData

postSharedCabRoutesDemand :: TokenAuthInfo -> SharedCabDemandReq -> Flow SharedCabDemandResp
postSharedCabRoutesDemand auth req = requireDriver auth >>= (`routesDemand` req)

postSharedCabRouteOperations :: TokenAuthInfo -> SharedCabRouteOperationReq -> Flow SelectRouteResp
postSharedCabRouteOperations auth req = requireDriver auth >>= (`routeOperation` req)

getSharedCabSession :: TokenAuthInfo -> Flow (Maybe SharedCabSession)
getSharedCabSession auth = requireDriver auth >>= currentSession

postSharedCabSessionOperations :: TokenAuthInfo -> SharedCabSessionOperationReq -> Flow SharedCabSession
postSharedCabSessionOperations auth req = requireDriver auth >>= (`sessionOperation` req)

postSharedCabBookingOperations :: TokenAuthInfo -> Text -> SharedCabBookingOperationReq -> Flow SharedCabSession
postSharedCabBookingOperations auth bookingId req = requireDriver auth >>= \driver -> bookingOperation driver bookingId req

postSharedCabBookingCall :: TokenAuthInfo -> Text -> Flow DCall.CallRes
postSharedCabBookingCall auth bookingId = requireDriver auth >>= (`callRider` bookingId)

postSharedCabBookingMessage :: TokenAuthInfo -> Text -> SharedCabMessageReq -> Flow APISuccess
postSharedCabBookingMessage auth bookingId req = requireDriver auth >>= \driver -> sendMessage driver bookingId req

getSharedCabCallStatus :: TokenAuthInfo -> Id SCS.CallStatus -> Flow DCall.GetCallStatusRes
getSharedCabCallStatus _ = DCall.getCallStatus

requireDriver :: TokenAuthInfo -> Flow DriverAuthInfo
requireDriver (mbPersonId, merchantId, merchantOpCityId) = do
  personId <- fromMaybeM (PersonNotFound "No person found") mbPersonId
  pure (personId, merchantId, merchantOpCityId)

sharedCabVehicleCategory :: Text
sharedCabVehicleCategory = "SHARED_CAB"

validateSharedCabDriver :: Id SP.Person -> Flow DVehicle.Vehicle
validateSharedCabDriver personId = do
  driver <- QPerson.findById personId >>= fromMaybeM (DriverNotFound personId.getId)
  unless (driver.role == SP.DRIVER) $ throwError (InvalidRequest ("Person " <> personId.getId <> " is not a driver"))
  vehicle <- QVehicle.findById personId >>= fromMaybeM (DriverWithoutVehicle personId.getId)
  unless (vehicle.variant == DV.SHARED_CAB) $
    throwError $
      InvalidRequest ("Vehicle variant " <> show vehicle.variant <> " is not a shared cab")
  pure vehicle

sharedCabBPPConfig :: Id MerchantOperatingCity -> Flow DIBC.IntegratedBPPConfig
sharedCabBPPConfig merchantOpCityId =
  findIntegratedBPPConfig Nothing merchantOpCityId sharedCabVehicleCategory APPLICATION

bapInternal :: Flow AppBackendBapInternal
bapInternal = asks (.appBackendBapInternal)

defaultCapacity :: Int
defaultCapacity = 6

routeStops :: DIBC.IntegratedBPPConfig -> Text -> Flow [RSM.RouteStopMapping]
routeStops integratedBPPConfig code = sortOn (.sequenceNum) <$> OTPRest.getRouteStopMappingByRouteCode code integratedBPPConfig

sessionStopOf :: RSM.RouteStopMapping -> SessionStop
sessionStopOf stop = SessionStop {code = stop.stopCode, name = stop.stopName, lat = stop.stopPoint.lat, lon = stop.stopPoint.lon}

lastStopName :: [RSM.RouteStopMapping] -> Text
lastStopName = maybe "" (.stopName) . listToMaybe . reverse

-- | The city's version of the feed, kept 5 min: every demand read carries it.
feedVersion :: DIBC.IntegratedBPPConfig -> Flow (Maybe Text)
feedVersion integratedBPPConfig =
  IM.withInMemCache ["SharedCabFeedVersion", integratedBPPConfig.agencyKey] 300 $
    either (const Nothing) Just <$> withTryCatch "getGtfsVersion:sharedCab" (OTPRest.getGtfsVersion integratedBPPConfig)

-- | The feed's routes straight from GIMS, for the driver app's local cache.
routeData :: DriverAuthInfo -> Flow SharedCabRouteDataResp
routeData (personId, _merchantId, merchantOpCityId) = do
  _ <- validateSharedCabDriver personId
  integratedBPPConfig <- sharedCabBPPConfig merchantOpCityId
  routes <- OTPRest.getRoutesByGtfsId integratedBPPConfig
  rows <- forM routes $ \route -> do
    stops <- routeStops integratedBPPConfig route.id
    pure
      SharedCabRoute
        { code = route.id,
          name = fromMaybe "" route.longName,
          direction = lastStopName stops,
          fromStop = maybe "" (.stopName) (listToMaybe stops),
          toStop = lastStopName stops,
          stops = map sessionStopOf stops,
          shapePolyline = route.encodedPolyline
        }
  publicTransportVersion <- feedVersion integratedBPPConfig
  pure SharedCabRouteDataResp {publicTransportVersion, routes = rows}

-- | A failure only leaves the app on its cached routes.
sharedCabFeedVersion :: Id MerchantOperatingCity -> Maybe DV.VehicleVariant -> Flow (Maybe Text)
sharedCabFeedVersion merchantOpCityId variant
  | variant /= Just DV.SHARED_CAB = pure Nothing
  | otherwise = either (const Nothing) identity <$> withTryCatch "sharedCabFeedVersion" (sharedCabBPPConfig merchantOpCityId >>= feedVersion)

routesDemand :: DriverAuthInfo -> SharedCabDemandReq -> Flow SharedCabDemandResp
routesDemand (personId, _merchantId, merchantOpCityId) req = do
  _ <- validateSharedCabDriver personId
  integratedBPPConfig <- sharedCabBPPConfig merchantOpCityId
  bap <- bapInternal
  SharedCabBAP.postSharedCabRoutesDemand bap.apiKey bap.url SharedCabBAP.BAPDemandReq {SharedCabBAP.agencyId = integratedBPPConfig.agencyKey, SharedCabBAP.routeCodes = req.routeCodes}

-- | Tells rider-app what changed (its live copy, releases and drops) and returns the driver's session view and the riders on board.
-- Never called back from rider-app's side of the sync, so holding the plate lock across it cannot deadlock.
syncCab :: Maybe CabChange -> Bool -> Session.Session -> Flow CabSyncResp
syncCab change notifyOnBoard s = do
  bap <- bapInternal
  SharedCabBAP.postCabSync bap.apiKey bap.url (Session.cabUpdate change notifyOnBoard s)

liveView :: CabSyncResp -> Flow SharedCabSession
liveView = fromMaybeM SessionNotFound . (.session)

affectedOf :: CabRider -> AffectedRider
affectedOf r = AffectedRider {bookingId = r.bookingId, dropStop = r.dropStop, firstName = r.firstName}

liftSession :: Either SharedCabSessionError a -> Flow a
liftSession = either throwError pure

ownSession :: Id SP.Person -> Text -> Flow Session.Session
ownSession personId plate = Session.readSession plate >>= liftSession . Session.ownedSession personId

routeOperation :: DriverAuthInfo -> SharedCabRouteOperationReq -> Flow SelectRouteResp
routeOperation auth req = case req.operation of
  SELECT_ROUTE -> do
    routeCode <- fromMaybeM (InvalidRequest "routeCode is required to select a route") req.routeCode
    walkupCount <- fromMaybeM (InvalidRequest "walkupCount is required to select a route") req.walkupCount
    selectSharedCabRoute auth req routeCode walkupCount
  _ -> endSharedCabRoute auth req

-- | Open a run, or change route if this driver already has one: a change with riders on board needs a mode. Rider-app hears of an
-- open before the trip row and the attach, so a suspended city refuses it before anything is written.
selectSharedCabRoute :: DriverAuthInfo -> SharedCabRouteOperationReq -> Text -> Int -> Flow SelectRouteResp
selectSharedCabRoute (personId, merchantId, merchantOpCityId) req routeCode walkupCount = do
  vehicle <- validateSharedCabDriver personId
  driver <- QPerson.findById personId >>= fromMaybeM (DriverNotFound personId.getId)
  integratedBPPConfig <- sharedCabBPPConfig merchantOpCityId
  void $ OTPRest.getRouteByRouteId integratedBPPConfig routeCode >>= fromMaybeM (RouteNotFound routeCode)
  let plate = vehicle.registrationNo
  selected <- Session.withPlateLock plate $ do
    prior <- Session.getSessionLocked plate
    liftSession (Session.planSelect personId routeCode prior) >>= \case
      Session.KeepRoute s -> Right <$> syncCab Nothing False s
      Session.ChangeRoute s -> do
        onBoard <- (.ridersOnBoard) <$> syncCab Nothing False s
        case req.mode of
          _ | null onBoard -> Right <$> (Session.switchTrip DVT.ROUTE_CHANGED routeCode s >>= syncCab (Just CAB_ROUTE_CHANGED) False)
          -- TODO: re-drop forced riders at the nearest common stop.
          Just Force -> Right <$> (Session.switchTrip DVT.ROUTE_CHANGED routeCode s >>= syncCab (Just CAB_ROUTE_CHANGED) True)
          Just AfterLastDrop -> Right <$> (Session.queueRoute routeCode s >>= syncCab (Just CAB_ROUTE_QUEUED) False)
          Nothing -> pure (Left onBoard)
      Session.OpenSession -> do
        now <- getCurrentTime
        tripId <- generateGUID
        let s =
              Session.newSession
                Session.NewSession
                  { Session.driverId = personId,
                    Session.vehicleNumber = plate,
                    Session.merchantId = merchantId,
                    Session.merchantOperatingCityId = merchantOpCityId,
                    Session.integratedBPPConfig = integratedBPPConfig,
                    Session.routeCode = routeCode,
                    Session.capacity = fromMaybe defaultCapacity vehicle.capacity,
                    Session.driverName = Just (unwords (catMaybes [Just driver.firstName, driver.lastName])),
                    Session.vehicleModel = Just vehicle.model
                  }
                tripId
                now
                prior
            undoOpen = void $ withTryCatch "sharedCab:undoOpen" (syncCab (Just CAB_ENDED) False (Session.endedCopy s))
        void $ syncCab (Just CAB_OPENED) False s
        Right <$> (Session.openTrip undoOpen s >>= syncCab Nothing False)
  case selected of
    Left onBoard -> pure SelectRouteResp {session = Nothing, affectedRiders = Just (map affectedOf onBoard)}
    Right resp -> do
      view <- liveView resp
      view' <-
        if walkupCount == view.walkupCount
          then pure view
          else
            setSeats personId plate $
              SharedCabBAP.BAPSessionOperationReq
                { SharedCabBAP.operation = SET_SEATS,
                  SharedCabBAP.driverId = personId.getId,
                  SharedCabBAP.vehicleNumber = plate,
                  SharedCabBAP.version = Just view.version,
                  SharedCabBAP.walkupCount = Just walkupCount
                }
      pure SelectRouteResp {session = Just view', affectedRiders = Nothing}

-- | The driver's session poll. A route queued for after the last drop is switched to here once no rider is left on board.
currentSession :: DriverAuthInfo -> Flow (Maybe SharedCabSession)
currentSession (personId, _merchantId, _merchantOpCityId) = do
  vehicle <- validateSharedCabDriver personId
  let plate = vehicle.registrationNo
  Session.getSession plate >>= \case
    Just s | s.status /= ENDED,
             s.driverId == personId -> do
      resp <- syncCab Nothing False s
      case s.queuedRouteCode of
        Just queued | null resp.ridersOnBoard -> Session.withPlateLock plate $ do
          fresh <- ownSession personId plate
          if fresh.queuedRouteCode == Just queued
            then (.session) <$> (Session.switchTrip DVT.ROUTE_CHANGED queued fresh >>= syncCab (Just CAB_ROUTE_CHANGED) False)
            else pure resp.session
        _ -> pure resp.session
    _ -> pure Nothing

endSharedCabRoute :: DriverAuthInfo -> SharedCabRouteOperationReq -> Flow SelectRouteResp
endSharedCabRoute (personId, _merchantId, _merchantOpCityId) req = do
  vehicle <- validateSharedCabDriver personId
  let plate = vehicle.registrationNo
      forced = req.forceEnd == Just True
  session <- Session.withPlateLock plate $ do
    s <- ownSession personId plate
    let guardOnBoard = do
          onBoard <- (.ridersOnBoard) <$> syncCab Nothing False s
          unless (forced || null onBoard) $ throwError (RidersOnBoard (length onBoard))
    case req.operation of
      CHANGE_ROUTE -> (.session) <$> syncCab Nothing False s
      RETURN_TRIP -> do
        guardOnBoard
        returnRoute <- liftSession (Session.returnRouteOf s.routeCode)
        (.session) <$> (Session.switchTrip DVT.RETURN returnRoute s >>= syncCab (Just CAB_ROUTE_CHANGED) True)
      _ -> do
        guardOnBoard
        let reason = if req.atLastStop == Just True then DVT.END_ROUTE else DVT.END_FOR_NOW
        Nothing <$ (Session.finishTrip reason s >>= syncCab (Just CAB_ENDED) False)
  pure SelectRouteResp {session, affectedRiders = Nothing}

-- | Walk-ups and cab full are seat inventory: rider-app decides them against the bookings; the walk-ups it adds are this trip's offline
-- boardings.
setSeats :: Id SP.Person -> Text -> SharedCabBAP.BAPSessionOperationReq -> Flow SharedCabSession
setSeats personId plate req = do
  bap <- bapInternal
  resp <- SharedCabBAP.postSessionOperation bap.apiKey bap.url req
  when (resp.walkupsAdded > 0) $
    Session.withPlateLock plate $ ownSession personId plate >>= void . Session.addOfflineBoardings resp.walkupsAdded
  pure resp.session

sessionOperation :: DriverAuthInfo -> SharedCabSessionOperationReq -> Flow SharedCabSession
sessionOperation (personId, _merchantId, _merchantOpCityId) req = do
  vehicle <- validateSharedCabDriver personId
  let plate = vehicle.registrationNo
  case req.operation of
    RESUME -> Session.withPlateLock plate $ do
      s <- ownSession personId plate
      Session.resume s >>= syncCab (Just CAB_RESUMED) False >>= liveView
    _ ->
      setSeats personId plate $
        SharedCabBAP.BAPSessionOperationReq
          { SharedCabBAP.operation = req.operation,
            SharedCabBAP.driverId = personId.getId,
            SharedCabBAP.vehicleNumber = plate,
            SharedCabBAP.version = req.version,
            SharedCabBAP.walkupCount = req.walkupCount
          }

data BookingAction = Cancel Text | BoardedWithoutCode | Dropped | Accept | Decline
  deriving (Show, Eq)

bookingActionOf :: SharedCabBookingOperationReq -> Maybe BookingAction
bookingActionOf req = case req.operation of
  ACCEPT -> Just Accept
  DECLINE -> Just Decline
  DROPPED -> Just Dropped
  BOARDED_WITHOUT_CODE -> Just BoardedWithoutCode
  CANCEL -> Cancel <$> req.reason

bookingOperation :: DriverAuthInfo -> Text -> SharedCabBookingOperationReq -> Flow SharedCabSession
bookingOperation (personId, _merchantId, merchantOpCityId) bookingId req = do
  action <- fromMaybeM (InvalidRequest "reason is required to cancel a booking") (bookingActionOf req)
  vehicle <- validateSharedCabDriver personId
  bap <- bapInternal
  session <-
    SharedCabBAP.postBookingOperation bap.apiKey bap.url bookingId $
      SharedCabBAP.BAPBookingOperationReq {operation = req.operation, driverId = personId.getId, vehicleNumber = vehicle.registrationNo, reason = actionReason action}
  -- the offer's answer goes into the driver's quote-response counters, like a ride request's
  whenJust (offerResponse action) $ DP.recordQuoteResponseCounters merchantOpCityId personId
  pure session

offerResponse :: BookingAction -> Maybe DCommon.SearchRequestForDriverResponse
offerResponse = \case
  Accept -> Just DCommon.Accept
  Decline -> Just DCommon.Reject
  _ -> Nothing

actionReason :: BookingAction -> Maybe Text
actionReason = \case
  Cancel reason -> Just reason
  _ -> Nothing

-- | Driver calls the rider of a shared-cab booking through Exotel, so each side sees only the ExoPhone. Same shape as
-- Domain.Action.UI.Call.initiateCallToCustomer, but keyed by the FRFS booking id: rider-app owns the booking and the rider's phone,
-- and decides whether the booking is inside the call window.
callRider :: DriverAuthInfo -> Text -> Flow DCall.CallRes
callRider (personId, merchantId, merchantOpCityId) bookingId = do
  vehicle <- validateSharedCabDriver personId
  bap <- bapInternal
  rider <- SharedCabBAP.getRiderCallNumber bap.apiKey bap.url bookingId personId.getId vehicle.registrationNo
  driverPhone <- getDriverPhone personId
  callStatusId <- generateGUID
  let entityId = DCall.sharedCabCallEntity bookingId
  exotelResponse <- TCall.initiateCall merchantId merchantOpCityId (DCall.driverCallReq driverPhone rider.riderNumber callStatusId entityId)
  logTagInfo ("BookingId: " <> bookingId) "Call initiated from shared-cab driver to rider."
  QCallStatus.create =<< DCall.buildDriverCallStatus callStatusId entityId merchantId merchantOpCityId exotelResponse
  pure $ DCall.CallRes callStatusId

getDriverPhone :: Id SP.Person -> Flow Text
getDriverPhone personId = do
  driver <- QPerson.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  decrypted <- mapM decrypt driver.mobileNumber
  fromMaybeM (InternalError "Driver has no phone number.") $ (<>) <$> driver.mobileCountryCode <*> decrypted

-- | The booking-keyed twin of POST /onMessage: rider-app checks the booking is on this driver's live cab and sends the rider the CHAT_MESSAGE push.
sendMessage :: DriverAuthInfo -> Text -> SharedCabMessageReq -> Flow APISuccess
sendMessage (personId, _, _) bookingId req = do
  vehicle <- validateSharedCabDriver personId
  bap <- bapInternal
  SharedCabBAP.postRiderMessage bap.apiKey bap.url bookingId SharedCabBAP.BAPMessageReq {driverId = personId.getId, vehicleNumber = vehicle.registrationNo, message = req.message}
