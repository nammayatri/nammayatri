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
    EditLocationReq (..),
    EditLocation (..),
    getDriverLoc,
    getRideStatus,
    editLocation,
  )
where

import qualified Beckn.ACL.Update as ACL
import qualified Beckn.Types.Core.Taxi.Common.Location as Common
import qualified Data.HashMap as HM
import qualified Domain.Types.Booking.Type as DB
import Domain.Types.Location (LocationAPIEntity, makeLocationAPIEntity)
import qualified Domain.Types.Location as DL
import qualified Domain.Types.LocationAddress as DLA
import qualified Domain.Types.LocationMapping as DLM
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Person as SPerson
import Domain.Types.Ride
import qualified Domain.Types.Ride as SRide
import Environment
import Kernel.Beam.Functions as B
import Kernel.External.Encryption
import qualified Kernel.External.Maps as Maps
import Kernel.Prelude hiding (HasField)
import Kernel.Storage.Esqueleto hiding (isNothing)
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.APISuccess (APISuccess (Success))
import Kernel.Types.Id
import Kernel.Utils.CalculateDistance (distanceBetweenInMeters)
import qualified Kernel.Utils.CalculateDistance as CD
import Kernel.Utils.Common
import qualified SharedLogic.CallBPP as CallBPP
import qualified SharedLogic.LocationMapping as SLM
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

data EditLocationReq = EditLocationReq
  { origin :: Maybe EditLocation,
    destination :: Maybe EditLocation
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)

data EditLocation = EditLocation
  { gps :: Maps.LatLong,
    address :: DLA.LocationAddress
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)

getDriverLoc ::
  ( CacheFlow m r,
    EncFlow m r,
    EsqDBFlow m r,
    EsqDBReplicaFlow m r,
    HasField "rideCfg" r RideConfig,
    HasFlowEnv m r '["internalEndPointHashMap" ::: HM.Map BaseUrl BaseUrl]
  ) =>
  Id SRide.Ride ->
  m GetDriverLocResp
getDriverLoc rideId = do
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
          Notify.notifyDriverOnTheWay booking.riderId
          Redis.setExp driverOnTheWay () driverOnTheWayNotifyExpiry
        when (isNothing mbHasReachedNotified && distance <= driverReachedDistance) $ do
          Notify.notifyDriverHasReached booking.riderId ride.otp ride.vehicleNumber
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
    HasField "rideCfg" r RideConfig,
    HasFlowEnv m r '["internalEndPointHashMap" ::: HM.Map BaseUrl BaseUrl]
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
          DB.RentalDetails _ -> Nothing
          DB.OneWaySpecialZoneDetails details -> Just $ makeLocationAPIEntity details.toLocation
          DB.DriverOfferDetails details -> Just $ makeLocationAPIEntity details.toLocation,
        ride = makeRideAPIEntity ride,
        customer = SPerson.makePersonAPIEntity decRider tag Nothing,
        driverPosition = mbPos <&> (.currPoint)
      }

editLocation ::
  ( CacheFlow m r,
    EncFlow m r,
    EsqDBFlow m r,
    MonadFlow m,
    HasField "shortDurationRetryCfg" r RetryCfg,
    HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    HasFlowEnv m r '["internalEndPointHashMap" ::: HM.Map BaseUrl BaseUrl]
  ) =>
  Id SRide.Ride ->
  (Id SPerson.Person, Id DM.Merchant) ->
  EditLocationReq ->
  m APISuccess
editLocation rideId (_, merchantId) req = do
  when (isNothing req.origin && isNothing req.destination) do
    throwError PickupOrDropLocationNotFound

  ride <- B.runInReplica $ QRide.findById rideId >>= fromMaybeM (RideNotFound rideId.getId)
  merchant <- CQMerchant.findById merchantId >>= fromMaybeM (MerchantNotFound merchantId.getId)

  let attemptsLeft = fromMaybe merchant.numOfAllowedEditPickupLocationAttemptsThreshold ride.allowedEditLocationAttempts
  when (attemptsLeft == 0) do
    throwError EditLocationAttemptsExhausted

  let bookingId = ride.bookingId
  booking <- B.runInReplica $ QRB.findById bookingId >>= fromMaybeM (BookingNotFound bookingId.getId)

  whenJust req.origin $ \pickup -> do
    when (ride.status /= SRide.NEW) do
      throwError (InvalidRequest $ "Customer is not allowed to change pickup as the ride is not NEW for rideId: " <> ride.id.getId)
    lastMapping <- QLM.findLastMapping ride.id.getId 0 >>= fromMaybeM (InternalError $ "Latest mapping not found for rideId: " <> ride.id.getId)
    initialLocationForRide <- QL.findById lastMapping.locationId >>= fromMaybeM (InternalError $ "Location not found for locationId:" <> lastMapping.locationId.getId)
    let initialLatLong = Maps.LatLong {lat = initialLocationForRide.lat, lon = initialLocationForRide.lon}
        currentLatLong = pickup.gps
    let distance = CD.distanceBetweenInMeters initialLatLong currentLatLong
    when (distance > merchant.editPickupDistanceThreshold) do
      throwError EditPickupLocationNotServiceable

    res <- try @_ @SomeException (CallBPP.callGetDriverLocation ride.trackingUrl)
    case res of
      Right res' -> do
        let curDriverLocation = res'.currPoint
        let distanceOfDriverFromChangingPickup = CD.distanceBetweenInMeters curDriverLocation currentLatLong
        when (distanceOfDriverFromChangingPickup < merchant.driverDistanceThresholdFromPickup) do
          throwError $ DriverAboutToReachAtInitialPickup (show distanceOfDriverFromChangingPickup)
      Left err -> do
        logTagInfo "DriverLocationFetchFailed" $ show err

    startLocation <- buildLocation pickup
    QL.create startLocation
    pickupMapForBooking <- SLM.buildPickUpLocationMapping startLocation.id bookingId.getId DLM.BOOKING (Just merchantId) ride.merchantOperatingCityId
    QLM.create pickupMapForBooking
    pickupMapForRide <- SLM.buildPickUpLocationMapping startLocation.id ride.id.getId DLM.RIDE (Just merchantId) ride.merchantOperatingCityId
    QLM.create pickupMapForRide

    let origin = Just $ mkDomainLocation pickup
    bppBookingId <- booking.bppBookingId & fromMaybeM (BookingFieldNotPresent "bppBookingId")
    let dUpdateReq =
          ACL.EditLocationBuildReq
            { bppRideId = ride.bppRideId,
              bppId = booking.providerId,
              bppUrl = booking.providerUrl,
              transactionId = booking.transactionId,
              destination = Nothing,
              ..
            }
    becknUpdateReq <- ACL.buildUpdateReq dUpdateReq
    void . withShortRetry $ CallBPP.update booking.providerUrl becknUpdateReq
    QRide.updateEditLocationAttempts ride.id (Just (attemptsLeft -1))
  pure Success

buildLocation :: MonadFlow m => EditLocation -> m DL.Location
buildLocation location = do
  guid <- generateGUID
  now <- getCurrentTime
  return $
    DL.Location
      { id = guid,
        createdAt = now,
        updatedAt = now,
        lat = location.gps.lat,
        lon = location.gps.lon,
        address = location.address
      }

mkDomainLocation :: EditLocation -> Common.Location
mkDomainLocation EditLocation {..} =
  Common.Location
    { gps =
        Common.Gps
          { lat = gps.lat,
            lon = gps.lon
          },
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
