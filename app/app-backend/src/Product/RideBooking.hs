module Product.RideBooking where

import App.Types
import Beckn.Types.Id
import qualified Data.Text as T
import EulerHS.Prelude hiding (id)
import qualified Storage.Queries.Organization as QOrg
import qualified Storage.Queries.SearchReqLocation as QLoc
import qualified Types.API.RideBooking as API
import Types.Error
import qualified Types.ProductInfo as Info
import qualified Types.Storage.Person as Person
import qualified Types.Storage.OldRide as SRide
import qualified Types.Storage.RideBooking as SRideBooking
import qualified Types.Storage.SearchReqLocation as SLoc
import Utils.Common
import qualified Storage.Queries.Ride as QRide

rideBookingStatus :: Id SRide.Ride -> Id Person.Person -> FlowHandler API.RideBookingStatusRes
rideBookingStatus rideBookingId personId = withFlowHandlerAPI $ do
  ride <- QRide.findById rideBookingId >>= fromMaybeM RideDoesNotExist
  unless (ride.personId == Just personId) $ throwError AccessDenied
  buildRideBookingStatusRes ride

rideBookingList :: Id Person.Person -> Maybe Integer -> Maybe Integer -> Maybe Bool -> FlowHandler API.RideBookingListRes
rideBookingList personId mbLimit mbOffset mbOnlyActive = withFlowHandlerAPI $ do
  rideList <- QRide.findAllByPerson personId mbLimit mbOffset mbOnlyActive
  API.RideBookingListRes <$> traverse buildRideBookingStatusRes rideList

buildRideBookingStatusRes :: DBFlow m r => SRide.Ride -> m API.RideBookingStatusRes
buildRideBookingStatusRes ride = do
  org <- QOrg.findOrganizationById ride.organizationId >>= fromMaybeM OrgNotFound
  fromLocId <- ride.fromLocation & fromMaybeM (RideFieldNotPresent "fromLocation")
  toLocId <- ride.toLocation & fromMaybeM (RideFieldNotPresent "toLocation")
  fromLocation <- QLoc.findLocationById fromLocId >>= fromMaybeM LocationNotFound
  toLocation <- QLoc.findLocationById toLocId >>= fromMaybeM LocationNotFound
  let rbStatus = case ride.status of
        SRide.INSTOCK -> SRideBooking.NEW
        SRide.CONFIRMED -> SRideBooking.CONFIRMED
        SRide.TRIP_ASSIGNED -> SRideBooking.TRIP_ASSIGNED
        SRide.COMPLETED -> SRideBooking.COMPLETED
        SRide.CANCELLED -> SRideBooking.CANCELLED
        _ -> SRideBooking.TRIP_ASSIGNED
  mbRide <-
    -- COMPLETED and TRIP_ASSIGNED means that there IS ride. But CANCELLED does not, it could be cancelled
    -- before TRIP_ASSIGNED. So technically there is a bug until we change our storage model.
    if rbStatus `elem` [SRideBooking.COMPLETED, SRideBooking.TRIP_ASSIGNED, SRideBooking.CANCELLED]
      then Just <$> buildRideAPIEntity ride
      else return Nothing

  return $
    API.RideBookingStatusRes
      { id = ride.id,
        status = rbStatus,
        agencyName = org.name,
        agencyNumber = org.mobileCountryCode <> org.mobileNumber,
        estimatedPrice = ride.price,
        toLocation = SLoc.makeSearchReqLocationAPIEntity toLocation,
        fromLocation = SLoc.makeSearchReqLocationAPIEntity fromLocation,
        ride = mbRide,
        createdAt = ride.createdAt,
        updatedAt = ride.updatedAt
      }

buildRideAPIEntity :: DBFlow m r => SRide.Ride -> m SRide.RideAPIEntity
buildRideAPIEntity ride = do
  let rideStatus = case ride.status of
        SRide.INPROGRESS -> SRide.INPROGRESS
        SRide.COMPLETED -> SRide.COMPLETED
        SRide.CANCELLED -> SRide.CANCELLED
        _ -> SRide.NEW
  info :: Info.ProductInfo <- ride.info >>= decodeFromText & fromMaybeM (InternalError "Unable to read product info.")
  return $
    SRide.RideAPIEntity
      { id = ride.id,
        shortRideId = ride.shortId,
        status = rideStatus,
        driverName = info.tracker >>= (.trip.driver) <&> (.name),
        driverNumber = info.tracker >>= (.trip.driver) <&> (.phones) >>= listToMaybe,
        driverRatings = info.tracker >>= (.trip.driver) >>= (.rating) <&> (.value) >>= readMaybe . T.unpack,
        driverRegisteredAt = info.tracker >>= (.trip.driver) <&> (.registeredAt),
        vehicleNumber = info.tracker >>= (.trip.vehicle) >>= (.registrationNumber),
        vehicleColor = info.tracker >>= (.trip.vehicle) >>= (.color),
        vehicleVariant = info.tracker >>= (.trip.vehicle) <&> (.variant),
        vehicleModel = info.tracker >>= (.trip.vehicle) >>= (.model),
        rideOtp = ride.udf4,
        computedPrice = ride.price,
        actualRideDistance = ride.actualDistance,
        createdAt = ride.createdAt,
        updatedAt = ride.updatedAt
      }
