{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.UI.Ride
  ( GetDriverLocResp,
    GetRideStatusResp (..),
    StopReq (..),
    getDriverLoc,
    getRideStatus,
    addStop,
    editStop,
  )
where

import qualified Beckn.ACL.Update as ACL
import qualified Beckn.Types.Core.Taxi.Common.Location as Common
import qualified Domain.Types.Booking.Type as DB
import Domain.Types.Location
import Domain.Types.LocationAddress
import qualified Domain.Types.LocationMapping as DLM
import Domain.Types.Merchant
import qualified Domain.Types.Person as SPerson
import Domain.Types.Ride
import qualified Domain.Types.Ride as SRide
import Environment
import Kernel.Beam.Functions as B
import Kernel.External.Encryption
import Kernel.External.Maps (LatLong)
import qualified Kernel.External.Maps as Maps
import Kernel.Prelude hiding (HasField)
import Kernel.Storage.Esqueleto hiding (isNothing)
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.APISuccess (APISuccess (Success))
import Kernel.Types.Id
import Kernel.Utils.CalculateDistance (distanceBetweenInMeters)
import Kernel.Utils.Common
import qualified SharedLogic.CallBPP as CallBPP
import qualified Storage.CachedQueries.Merchant as CQMerchant
import qualified Storage.Queries.Booking as QRB
import qualified Storage.Queries.Location as QL
import qualified Storage.Queries.LocationMapping as QLM
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.Person.PersonDisability as PDisability
import qualified Storage.Queries.Ride as QRide
import Tools.Error
import qualified Tools.Maps as MapSearch
import qualified Tools.Notifications as Notify

data GetDriverLocResp = GetDriverLocResp
  { lat :: Double,
    lon :: Double,
    lastUpdate :: UTCTime
  }
  deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

data GetRideStatusResp = GetRideStatusResp
  { fromLocation :: LocationAPIEntity,
    toLocation :: Maybe LocationAPIEntity,
    ride :: RideAPIEntity,
    customer :: SPerson.PersonAPIEntity,
    driverPosition :: Maybe MapSearch.LatLong
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)

data StopReq = StopReq
  { gps :: LatLong,
    address :: LocationAddress
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)

addStop ::
  ( CacheFlow m r,
    EncFlow m r,
    HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    HasField "shortDurationRetryCfg" r RetryCfg,
    EsqDBFlow m r,
    MonadFlow m
  ) =>
  (Id SPerson.Person, Id Merchant) ->
  Id SRide.Ride ->
  StopReq ->
  m APISuccess
addStop (_, merchantId) rideId req = do
  ride <- B.runInReplica $ QRide.findById rideId >>= fromMaybeM (RideDoesNotExist rideId.getId)
  booking <- B.runInReplica $ QRB.findById ride.bookingId >>= fromMaybeM (BookingNotFound ride.bookingId.getId)
  validateStopReq ride booking False
  case ride.nextStopLocId of
    Just _ -> throwError $ InvalidRequest "Can't add stop as previous stop not reached yet"
    Nothing -> processStop ride booking req merchantId False
  pure Success

editStop ::
  ( CacheFlow m r,
    EncFlow m r,
    HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    HasField "shortDurationRetryCfg" r RetryCfg,
    EsqDBFlow m r,
    MonadFlow m
  ) =>
  (Id SPerson.Person, Id Merchant) ->
  Id SRide.Ride ->
  StopReq ->
  m APISuccess
editStop (_, merchantId) rideId req = do
  ride <- B.runInReplica $ QRide.findById rideId >>= fromMaybeM (RideDoesNotExist rideId.getId)
  booking <- B.runInReplica $ QRB.findById ride.bookingId >>= fromMaybeM (BookingNotFound ride.bookingId.getId)
  validateStopReq ride booking True
  case ride.nextStopLocId of
    Nothing -> throwError $ InvalidRequest "Can't add stop as previous stop not reached yet"
    Just _ -> processStop ride booking req merchantId True
  pure Success

processStop ::
  ( MonadFlow m,
    CacheFlow m r,
    EsqDBFlow m r,
    HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    HasField "shortDurationRetryCfg" r RetryCfg
  ) =>
  Ride ->
  DB.Booking ->
  StopReq ->
  Id Merchant ->
  Bool ->
  m ()
processStop ride booking loc merchantId isEdit = do
  location <- buildLocation loc
  locationMapping <- buildLocationMapping ride.id location.id isEdit
  QL.create location
  QLM.create locationMapping
  QRide.updateNextStop ride.id (Just location.id.getId)
  bppBookingId <- booking.bppBookingId & fromMaybeM (BookingFieldNotPresent "bppBookingId")
  merchant <- CQMerchant.findById merchantId >>= fromMaybeM (MerchantNotFound merchantId.getId)
  let dUpdateReq =
        if isEdit
          then
            ACL.EditStopBuildReq
              { bppRideId = ride.bppRideId,
                bppId = booking.providerId,
                bppUrl = booking.providerUrl,
                transactionId = booking.transactionId,
                stops = [mkDomainLocation location],
                ..
              }
          else
            ACL.AddStopBuildReq
              { bppRideId = ride.bppRideId,
                bppId = booking.providerId,
                bppUrl = booking.providerUrl,
                transactionId = booking.transactionId,
                stops = [mkDomainLocation location],
                ..
              }
  becknUpdateReq <- ACL.buildUpdateReq dUpdateReq
  void . withShortRetry $ CallBPP.update booking.providerUrl becknUpdateReq

validateStopReq :: (MonadFlow m) => Ride -> DB.Booking -> Bool -> m ()
validateStopReq ride booking isEdit = do
  unless (ride.status `elem` [INPROGRESS, NEW]) $ throwError (RideInvalidStatus $ "Cannot edit/add stop in this ride " <> ride.id.getId)
  if isEdit
    then unless (isJust ride.nextStopLocId) $ throwError (InvalidRequest $ "Can't find stop to be edited " <> ride.id.getId) -- should we throw error or just allow?
    else unless (isNothing ride.nextStopLocId) $ throwError (InvalidRequest $ "Can't add next stop before reaching previous stop " <> ride.id.getId)
  case booking.bookingDetails of
    DB.OneWayDetails _ -> throwError $ RideInvalidStatus "Cannot add/edit stop in on demand rides"
    DB.RentalDetails _ _ -> pure ()
    DB.DriverOfferDetails _ -> throwError $ RideInvalidStatus "Cannot add/edit stop in on demand rides"
    DB.OneWaySpecialZoneDetails _ -> throwError $ RideInvalidStatus "Cannot add/edit stop in special zone rides"

buildLocationMapping :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id Ride -> Id Location -> Bool -> m DLM.LocationMapping
buildLocationMapping rideId locationId isEdit = do
  id <- generateGUID
  prevOrder <- QLM.maxOrderByEntity rideId.getId
  let version = "LATEST"
  when isEdit $ QLM.updatePastMappingVersions rideId.getId prevOrder
  return $
    DLM.LocationMapping
      { entityId = rideId.getId,
        tag = DLM.RIDE,
        order = if isEdit then prevOrder else prevOrder + 1,
        ..
      }

buildLocation :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => StopReq -> m Location
buildLocation req = do
  id <- generateGUID
  now <- getCurrentTime
  return $
    Location
      { lat = req.gps.lat,
        lon = req.gps.lon,
        address = req.address,
        createdAt = now,
        updatedAt = now,
        ..
      }

mkDomainLocation :: Location -> Common.Location
mkDomainLocation Location {..} =
  Common.Location
    { gps = Common.Gps {..},
      address =
        Common.Address
          { locality = address.area,
            area_code = address.areaCode,
            state = address.state,
            country = address.country,
            building = address.building,
            street = address.street,
            city = address.city,
            ward = address.ward,
            door = address.door
          }
    }

getDriverLoc ::
  ( CacheFlow m r,
    EncFlow m r,
    EsqDBFlow m r,
    EsqDBReplicaFlow m r,
    HasField "rideCfg" r RideConfig
  ) =>
  Id SRide.Ride ->
  Id SPerson.Person ->
  m GetDriverLocResp
getDriverLoc rideId personId = do
  ride <- B.runInReplica $ QRide.findById rideId >>= fromMaybeM (RideDoesNotExist rideId.getId)
  when
    (ride.status == COMPLETED || ride.status == CANCELLED)
    $ throwError $ RideInvalidStatus "Cannot track this ride"
  res <- CallBPP.callGetDriverLocation ride.trackingUrl
  booking <- B.runInReplica $ QRB.findById ride.bookingId >>= fromMaybeM (BookingDoesNotExist ride.bookingId.getId)
  let fromLocation = Maps.getCoordinates booking.fromLocation
  driverReachedDistance <- asks (.rideCfg.driverReachedDistance)
  driverOnTheWayNotifyExpiry <- getSeconds <$> asks (.rideCfg.driverOnTheWayNotifyExpiry)
  mbIsOnTheWayNotified <- Redis.get @() driverOnTheWay
  mbHasReachedNotified <- Redis.get @() driverHasReached
  when (ride.status == NEW && (isNothing mbIsOnTheWayNotified || isNothing mbHasReachedNotified)) $ do
    let distance = highPrecMetersToMeters $ distanceBetweenInMeters fromLocation res.currPoint
    mbStartDistance <- Redis.get @Meters distanceUpdates
    case mbStartDistance of
      Nothing -> Redis.setExp distanceUpdates distance 3600
      Just startDistance -> when (startDistance - 50 > distance) $ do
        unless (isJust mbIsOnTheWayNotified) $ do
          Notify.notifyDriverOnTheWay personId
          Redis.setExp driverOnTheWay () driverOnTheWayNotifyExpiry
        when (isNothing mbHasReachedNotified && distance <= driverReachedDistance) $ do
          Notify.notifyDriverHasReached personId ride.otp ride.vehicleNumber
          Redis.setExp driverHasReached () 1500
  return $
    GetDriverLocResp
      { lat = res.currPoint.lat,
        lon = res.currPoint.lon,
        lastUpdate = res.lastUpdate
      }
  where
    distanceUpdates = "Ride:GetDriverLoc:DriverDistance " <> rideId.getId
    driverOnTheWay = "Ride:GetDriverLoc:DriverIsOnTheWay " <> rideId.getId
    driverHasReached = "Ride:GetDriverLoc:DriverHasReached " <> rideId.getId

getRideStatus ::
  ( CacheFlow m r,
    EncFlow m r,
    EsqDBFlow m r,
    EsqDBReplicaFlow m r,
    HasField "rideCfg" r RideConfig
  ) =>
  Id SRide.Ride ->
  Id SPerson.Person ->
  m GetRideStatusResp
getRideStatus rideId personId = withLogTag ("personId-" <> personId.getId) do
  ride <- B.runInReplica $ QRide.findById rideId >>= fromMaybeM (RideDoesNotExist rideId.getId)
  mbPos <-
    if ride.status == COMPLETED || ride.status == CANCELLED
      then return Nothing
      else Just <$> CallBPP.callGetDriverLocation ride.trackingUrl
  booking <- B.runInReplica $ QRB.findById ride.bookingId >>= fromMaybeM (BookingDoesNotExist ride.bookingId.getId)
  rider <- B.runInReplica $ QP.findById booking.riderId >>= fromMaybeM (PersonNotFound booking.riderId.getId)
  customerDisability <- B.runInReplica $ PDisability.findByPersonId personId
  let tag = customerDisability <&> (.tag)
  decRider <- decrypt rider
  return $
    GetRideStatusResp
      { fromLocation = makeLocationAPIEntity booking.fromLocation,
        toLocation = case booking.bookingDetails of
          DB.OneWayDetails details -> Just $ makeLocationAPIEntity details.toLocation
          DB.RentalDetails _ _ -> Nothing
          DB.OneWaySpecialZoneDetails details -> Just $ makeLocationAPIEntity details.toLocation
          DB.DriverOfferDetails details -> Just $ makeLocationAPIEntity details.toLocation,
        ride = makeRideAPIEntity ride,
        customer = SPerson.makePersonAPIEntity decRider tag,
        driverPosition = mbPos <&> (.currPoint)
      }
