module Domain.Action.UI.Ride
  ( DriverRideRes (..),
    DriverRideListRes (..),
    listDriverRides,
    arrivedAtPickup,
  )
where

import Beckn.External.Maps (HasCoordinates (getCoordinates))
import Beckn.External.Maps.Types
import Beckn.Prelude
import qualified Beckn.Storage.Esqueleto as Esq
import Beckn.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import Beckn.Storage.Esqueleto.Transactionable (runInReplica)
import Beckn.Tools.Metrics.CoreMetrics.Types
import Beckn.Types.APISuccess
import Beckn.Types.Common
import Beckn.Types.Id
import Beckn.Utils.CalculateDistance (distanceBetweenInMeters)
import Beckn.Utils.Common
import qualified Domain.Types.Booking as DRB
import qualified Domain.Types.Booking.BookingLocation as DBLoc
import qualified Domain.Types.Driver.DriverFlowStatus as DDFS
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Ride as DRide
import qualified Domain.Types.RideDetails as RD
import qualified Domain.Types.Vehicle as DVeh
import qualified SharedLogic.CallBAP as BP
import SharedLogic.FareCalculator
import Storage.CachedQueries.CacheConfig
import qualified Storage.Queries.Booking as QBooking
import qualified Storage.Queries.Driver.DriverFlowStatus as QDFS
import qualified Storage.Queries.Ride as QRide
import qualified Storage.Queries.RideDetails as QRD
import Tools.Error

data DriverRideRes = DriverRideRes
  { id :: Id DRide.Ride,
    shortRideId :: ShortId DRide.Ride,
    status :: DRide.RideStatus,
    fromLocation :: DBLoc.BookingLocationAPIEntity,
    toLocation :: DBLoc.BookingLocationAPIEntity,
    driverName :: Text,
    driverNumber :: Maybe Text,
    vehicleVariant :: DVeh.Variant,
    vehicleModel :: Text,
    vehicleColor :: Text,
    vehicleNumber :: Text,
    computedFare :: Maybe Money,
    estimatedBaseFare :: Money,
    estimatedDistance :: Meters,
    driverSelectedFare :: Money,
    actualRideDistance :: HighPrecMeters,
    rideRating :: Maybe Int,
    createdAt :: UTCTime,
    updatedAt :: UTCTime,
    riderName :: Maybe Text,
    tripStartTime :: Maybe UTCTime,
    tripEndTime :: Maybe UTCTime,
    chargeableDistance :: Maybe Meters
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

newtype DriverRideListRes = DriverRideListRes
  { list :: [DriverRideRes]
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

listDriverRides ::
  (EsqDBReplicaFlow m r, EncFlow m r) =>
  Id DP.Person ->
  Maybe Integer ->
  Maybe Integer ->
  Maybe Bool ->
  Maybe DRide.RideStatus ->
  m DriverRideListRes
listDriverRides driverId mbLimit mbOffset mbOnlyActive mbRideStatus = do
  rides <- runInReplica $ QRide.findAllByDriverId driverId mbLimit mbOffset mbOnlyActive mbRideStatus
  driverRideLis <- forM rides $ \(ride, booking) -> do
    rideDetail <- runInReplica $ QRD.findById ride.id >>= fromMaybeM (VehicleNotFound driverId.getId)
    driverNumber <- RD.getDriverNumber rideDetail
    pure $ mkDriverRideRes rideDetail driverNumber (ride, booking)
  pure . DriverRideListRes $ driverRideLis

mkDriverRideRes ::
  RD.RideDetails ->
  Maybe Text ->
  (DRide.Ride, DRB.Booking) ->
  DriverRideRes
mkDriverRideRes rideDetails driverNumber (ride, booking) = do
  let fareParams = booking.fareParams
  let initial = "" :: Text
  DriverRideRes
    { id = ride.id,
      shortRideId = ride.shortId,
      status = ride.status,
      fromLocation = DBLoc.makeBookingLocationAPIEntity booking.fromLocation,
      toLocation = DBLoc.makeBookingLocationAPIEntity booking.toLocation,
      driverName = rideDetails.driverName,
      driverNumber,
      vehicleNumber = rideDetails.vehicleNumber,
      vehicleColor = fromMaybe initial rideDetails.vehicleColor,
      vehicleVariant = fromMaybe DVeh.SEDAN rideDetails.vehicleVariant,
      vehicleModel = fromMaybe initial rideDetails.vehicleModel,
      computedFare = ride.fare,
      estimatedBaseFare = baseFareSum fareParams,
      estimatedDistance = booking.estimatedDistance,
      driverSelectedFare = fromMaybe 0 fareParams.driverSelectedFare,
      actualRideDistance = ride.traveledDistance,
      createdAt = ride.createdAt,
      updatedAt = ride.updatedAt,
      riderName = booking.riderName,
      tripStartTime = ride.tripStartTime,
      tripEndTime = ride.tripEndTime,
      rideRating = ride.rideRating <&> (.ratingValue),
      chargeableDistance = ride.chargeableDistance
    }

arrivedAtPickup :: (EncFlow m r, CacheFlow m r, EsqDBFlow m r, EsqDBReplicaFlow m r, CoreMetrics m, HasShortDurationRetryCfg r c, HasFlowEnv m r '["nwAddress" ::: BaseUrl], HasHttpClientOptions r c, HasFlowEnv m r '["driverReachedDistance" ::: HighPrecMeters]) => Id DRide.Ride -> LatLong -> m APISuccess
arrivedAtPickup rideId req = do
  ride <- runInReplica $ QRide.findById rideId >>= fromMaybeM (RideDoesNotExist rideId.getId)
  unless (isValidRideStatus (ride.status)) $ throwError $ RideInvalidStatus "The ride has already started."
  booking <- runInReplica $ QBooking.findById ride.bookingId >>= fromMaybeM (BookingDoesNotExist ride.bookingId.getId)
  let pickupLoc = getCoordinates booking.fromLocation
  let distance = distanceBetweenInMeters req pickupLoc
  driverReachedDistance <- asks (.driverReachedDistance)
  unless (distance < driverReachedDistance) $ throwError $ DriverNotAtPickupLocation ride.driverId.getId
  unless (isJust ride.driverArrivalTime) $ do
    Esq.runTransaction $ do
      QRide.updateArrival rideId
      QDFS.updateStatus ride.driverId DDFS.WAITING_FOR_CUSTOMER {rideId}
    BP.sendDriverArrivalUpdateToBAP booking ride ride.driverArrivalTime
  pure Success
  where
    isValidRideStatus status = status == DRide.NEW
