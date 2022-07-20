module Domain.Action.Beckn.Init where

import Beckn.External.GoogleMaps.Types
import Beckn.Prelude
import qualified Beckn.Product.MapSearch as MapSearch
import Beckn.Serviceability (rideServiceable)
import qualified Beckn.Storage.Esqueleto as DB
import Beckn.Tools.Metrics.CoreMetrics
import Beckn.Types.Amount (Amount)
import Beckn.Types.Geofencing
import Beckn.Types.Id
import Beckn.Types.MapSearch (LatLong (..))
import qualified Beckn.Types.MapSearch as MapSearch
import qualified Domain.Types.BookingLocation as DLoc
import qualified Domain.Types.Organization as DOrg
import qualified Domain.Types.RideBooking as DRB
import qualified Domain.Types.Vehicle as Veh
import SharedLogic.FareCalculator.OneWayFareCalculator
import qualified Storage.Queries.BookingLocation as QBLoc
import qualified Storage.Queries.FarePolicy.RentalFarePolicy as QRFP
import qualified Storage.Queries.Geometry as QGeometry
import qualified Storage.Queries.Organization as QOrg
import qualified Storage.Queries.RideBooking as QRB
import Types.Error
import Utils.Common

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
  { rideBooking :: DRB.RideBooking,
    transporter :: DOrg.Organization
  }

init ::
  ( EsqDBFlow m r,
    HasField "geofencingConfig" r GeofencingConfig,
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
  rideBooking <- case req.initTypeReq of
    InitOneWayTypeReq oneWayReq -> do
      initOneWayTrip req oneWayReq transporter.id now
    InitRentalTypeReq rentalReq -> do
      initRentalTrip req rentalReq transporter.id now
  return $
    InitRes
      { rideBooking = rideBooking,
        transporter = transporter
      }

initOneWayTrip ::
  ( EsqDBFlow m r,
    HasField "geofencingConfig" r GeofencingConfig,
    CoreMetrics m,
    HasGoogleMaps m r
  ) =>
  InitReq ->
  InitOneWayReq ->
  Id DOrg.Organization ->
  UTCTime ->
  m DRB.RideBooking
initOneWayTrip req oneWayReq transporterId now = do
  unlessM (rideServiceable QGeometry.someGeometriesContain req.fromLocation (Just oneWayReq.toLocation)) $
    throwError RideNotServiceable
  distance <-
    metersToHighPrecMeters . (.distance) <$> MapSearch.getDistance (Just MapSearch.CAR) req.fromLocation oneWayReq.toLocation
  fareParams <- calculateFare transporterId req.vehicleVariant distance req.startTime
  toLoc <- buildRBLoc oneWayReq.toLocation now
  let estimatedFare = fareSum fareParams
      discount = fareParams.discount
      estimatedTotalFare = fareSumWithDiscount fareParams
      owDetails =
        DRB.OneWayDetails $
          DRB.OneWayRideBookingDetails
            { DRB.toLocationId = toLoc.id,
              DRB.estimatedDistance = distance
            }
  fromLoc <- buildRBLoc req.fromLocation now
  rideBooking <- buildRideBooking req transporterId estimatedFare discount estimatedTotalFare owDetails fromLoc.id now
  DB.runTransaction $ do
    QBLoc.create fromLoc
    QBLoc.create toLoc
    QRB.create rideBooking
  return rideBooking

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
  m DRB.RideBooking
initRentalTrip req rentalReq transporterId now = do
  unlessM (rideServiceable QGeometry.someGeometriesContain req.fromLocation Nothing) $
    throwError RideNotServiceable
  let estimatedFare = 0
      discount = Nothing
      estimatedTotalFare = 0
  rentalFarePolicy <- QRFP.findByOffer transporterId req.vehicleVariant rentalReq.distance rentalReq.duration >>= fromMaybeM NoFarePolicy
  let rentDetails = DRB.RentalDetails $ DRB.RentalRideBookingDetails {rentalFarePolicyId = rentalFarePolicy.id}
  fromLoc <- buildRBLoc req.fromLocation now
  rideBooking <- buildRideBooking req transporterId estimatedFare discount estimatedTotalFare rentDetails fromLoc.id now
  DB.runTransaction $ do
    QBLoc.create fromLoc
    QRB.create rideBooking
  return rideBooking

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
              door = Nothing,
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

buildRideBooking ::
  MonadFlow m =>
  InitReq ->
  Id DOrg.Organization ->
  Amount ->
  Maybe Amount ->
  Amount ->
  DRB.RideBookingDetails ->
  Id DLoc.BookingLocation ->
  UTCTime ->
  m DRB.RideBooking
buildRideBooking req orgId estimatedFare discount estimatedTotalFare rideBookingDetails fromLocationId now = do
  id <- generateGUID
  return $
    DRB.RideBooking
      { id = Id id,
        status = DRB.NEW,
        providerId = orgId,
        startTime = req.startTime,
        riderId = Nothing,
        fromLocationId = fromLocationId,
        bapId = req.bapId,
        bapUri = req.bapUri,
        estimatedFare = estimatedFare,
        discount = discount,
        estimatedTotalFare = estimatedTotalFare,
        vehicleVariant = req.vehicleVariant,
        reallocationsCount = 0,
        rideBookingDetails = rideBookingDetails,
        createdAt = now,
        updatedAt = now
      }
