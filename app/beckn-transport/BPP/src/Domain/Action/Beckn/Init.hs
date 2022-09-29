module Domain.Action.Beckn.Init where

import Beckn.External.GoogleMaps.Types
import Beckn.Prelude
import qualified Beckn.Product.MapSearch as MapSearch
import Beckn.Serviceability
import qualified Beckn.Storage.Esqueleto as DB
import Beckn.Tools.Metrics.CoreMetrics
import Beckn.Types.Geofencing
import Beckn.Types.Id
import Beckn.Types.MapSearch (LatLong (..))
import qualified Beckn.Types.MapSearch as MapSearch
import Beckn.Utils.Common
import qualified Domain.Types.Booking as DRB
import qualified Domain.Types.Booking.BookingLocation as DLoc
import qualified Domain.Types.Organization as DOrg
import qualified Domain.Types.Vehicle as Veh
import SharedLogic.FareCalculator.OneWayFareCalculator
import qualified Storage.Queries.Booking as QRB
import qualified Storage.Queries.FarePolicy.RentalFarePolicy as QRFP
import qualified Storage.Queries.Geometry as QGeometry
import qualified Storage.Queries.Organization as QOrg
import Tools.Error

data InitReq = InitReq
  { vehicleVariant :: Veh.Variant,
    fromLocation :: LatLong,
    startTime :: UTCTime,
    bapId :: Text,
    bapUri :: BaseUrl,
    initTypeReq :: InitTypeReq
  }

data InitTypeReq = InitOneWayTypeReq InitOneWayReq | InitRentalTypeReq InitRentalReq

newtype InitOneWayReq = InitOneWayReq
  { toLocation :: LatLong
  }

data InitRentalReq = InitRentalReq
  { distance :: Kilometers,
    duration :: Hours
  }

data InitRes = InitRes
  { booking :: DRB.Booking,
    transporter :: DOrg.Organization
  }

init ::
  ( EsqDBFlow m r,
    HasField "geofencingConfig" r GeofencingConfig,
    HasField "driverEstimatedPickupDuration" r Seconds,
    CoreMetrics m,
    HasGoogleMaps m r
  ) =>
  Id DOrg.Organization ->
  InitReq ->
  m InitRes
init transporterId req = do
  transporter <- QOrg.findById transporterId >>= fromMaybeM (OrgDoesNotExist transporterId.getId)
  unless transporter.enabled $ throwError AgencyDisabled
  now <- getCurrentTime
  booking <- case req.initTypeReq of
    InitOneWayTypeReq oneWayReq -> do
      initOneWayTrip req oneWayReq transporter.id now
    InitRentalTypeReq rentalReq -> do
      initRentalTrip req rentalReq transporter.id now
  return $
    InitRes
      { booking = booking,
        transporter = transporter
      }

initOneWayTrip ::
  ( EsqDBFlow m r,
    HasField "geofencingConfig" r GeofencingConfig,
    HasField "driverEstimatedPickupDuration" r Seconds,
    CoreMetrics m,
    HasGoogleMaps m r
  ) =>
  InitReq ->
  InitOneWayReq ->
  Id DOrg.Organization ->
  UTCTime ->
  m DRB.Booking
initOneWayTrip req oneWayReq transporterId now = do
  unlessM (rideServiceableDefault QGeometry.someGeometriesContain req.fromLocation (Just oneWayReq.toLocation)) $
    throwError RideNotServiceable
  driverEstimatedPickupDuration <- asks (.driverEstimatedPickupDuration)
  distRes <- MapSearch.getDistance (Just MapSearch.CAR) req.fromLocation oneWayReq.toLocation
  let distance = distRes.distance
      estimatedRideDuration = distRes.duration_in_traffic
      estimatedRideFinishTime = realToFrac (driverEstimatedPickupDuration + estimatedRideDuration) `addUTCTime` req.startTime
  fareParams <- calculateFare transporterId req.vehicleVariant distance estimatedRideFinishTime
  toLoc <- buildRBLoc oneWayReq.toLocation now
  let estimatedFare = fareSum fareParams
      discount = fareParams.discount
      estimatedTotalFare = fareSumWithDiscount fareParams
      owDetails =
        DRB.OneWayDetails $
          DRB.OneWayBookingDetails
            { DRB.toLocation = toLoc,
              DRB.estimatedDistance = distance,
              DRB.estimatedFinishTime = estimatedRideFinishTime
            }
  fromLoc <- buildRBLoc req.fromLocation now
  booking <- buildBooking req transporterId estimatedFare discount estimatedTotalFare owDetails fromLoc now
  DB.runTransaction $ do
    QRB.create booking
  return booking

initRentalTrip ::
  ( EsqDBFlow m r,
    HasField "geofencingConfig" r GeofencingConfig,
    CoreMetrics m,
    HasGoogleMaps m r
  ) =>
  InitReq ->
  InitRentalReq ->
  Id DOrg.Organization ->
  UTCTime ->
  m DRB.Booking
initRentalTrip req rentalReq transporterId now = do
  unlessM (rideServiceableDefault QGeometry.someGeometriesContain req.fromLocation Nothing) $
    throwError RideNotServiceable
  let estimatedFare = 0
      discount = Nothing
      estimatedTotalFare = 0
  rentalFarePolicy <- QRFP.findByOffer transporterId req.vehicleVariant rentalReq.distance rentalReq.duration >>= fromMaybeM NoFarePolicy
  let rentDetails = DRB.RentalDetails $ DRB.RentalBookingDetails {rentalFarePolicyId = rentalFarePolicy.id}
  fromLoc <- buildRBLoc req.fromLocation now
  booking <- buildBooking req transporterId estimatedFare discount estimatedTotalFare rentDetails fromLoc now
  DB.runTransaction $ do
    QRB.create booking
  return booking

buildRBLoc ::
  (MonadFlow m) =>
  LatLong ->
  UTCTime ->
  m DLoc.BookingLocation
buildRBLoc latLon now = do
  locId <- generateGUID
  return
    DLoc.BookingLocation
      { id = locId,
        lat = latLon.lat,
        lon = latLon.lon,
        address =
          DLoc.LocationAddress
            { street = Nothing,
              city = Nothing,
              state = Nothing,
              country = Nothing,
              building = Nothing,
              areaCode = Nothing,
              area = Nothing
            },
        createdAt = now,
        updatedAt = now
      }

buildBooking ::
  MonadFlow m =>
  InitReq ->
  Id DOrg.Organization ->
  Money ->
  Maybe Money ->
  Money ->
  DRB.BookingDetails ->
  DLoc.BookingLocation ->
  UTCTime ->
  m DRB.Booking
buildBooking req orgId estimatedFare discount estimatedTotalFare bookingDetails fromLocation now = do
  id <- generateGUID
  return $
    DRB.Booking
      { id = Id id,
        status = DRB.NEW,
        providerId = orgId,
        startTime = req.startTime,
        riderId = Nothing,
        fromLocation,
        bapId = req.bapId,
        bapUri = req.bapUri,
        estimatedFare = estimatedFare,
        discount = discount,
        estimatedTotalFare = estimatedTotalFare,
        vehicleVariant = req.vehicleVariant,
        reallocationsCount = 0,
        bookingDetails = bookingDetails,
        riderName = Nothing,
        createdAt = now,
        updatedAt = now
      }
