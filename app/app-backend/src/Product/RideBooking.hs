module Product.RideBooking where

import App.Types
import Beckn.Types.Id
import qualified Data.Text as T
import EulerHS.Prelude hiding (id)
import qualified Storage.Queries.Organization as QOrg
import qualified Storage.Queries.ProductInstance as QPI
import qualified Storage.Queries.SearchReqLocation as QLoc
import qualified Types.API.RideBooking as API
import Types.Error
import qualified Types.ProductInfo as Info
import qualified Types.Storage.Person as Person
import qualified Types.Storage.ProductInstance as SPI
import qualified Types.Storage.Ride as SRide
import qualified Types.Storage.RideBooking as SRideBooking
import qualified Types.Storage.SearchReqLocation as SLoc
import Utils.Common

rideBookingStatus :: Id SPI.ProductInstance -> Id Person.Person -> FlowHandler API.RideBookingStatusRes
rideBookingStatus rideBookingId personId = withFlowHandlerAPI $ do
  orderPI <- QPI.findById rideBookingId >>= fromMaybeM PIDoesNotExist
  unless (orderPI.personId == Just personId) $ throwError AccessDenied
  buildRideBookingStatusRes orderPI

rideBookingList :: Id Person.Person -> Maybe Integer -> Maybe Integer -> Maybe Bool -> FlowHandler API.RideBookingListRes
rideBookingList personId mbLimit mbOffset mbOnlyActive = withFlowHandlerAPI $ do
  orderPIList <- QPI.findAllOrdersByPerson personId mbLimit mbOffset mbOnlyActive
  API.RideBookingListRes <$> traverse buildRideBookingStatusRes orderPIList

buildRideBookingStatusRes :: DBFlow m r => SPI.ProductInstance -> m API.RideBookingStatusRes
buildRideBookingStatusRes orderPI = do
  org <- QOrg.findOrganizationById orderPI.organizationId >>= fromMaybeM OrgNotFound
  fromLocId <- orderPI.fromLocation & fromMaybeM (PIFieldNotPresent "fromLocation")
  toLocId <- orderPI.toLocation & fromMaybeM (PIFieldNotPresent "toLocation")
  fromLocation <- QLoc.findLocationById fromLocId >>= fromMaybeM LocationNotFound
  toLocation <- QLoc.findLocationById toLocId >>= fromMaybeM LocationNotFound
  let rbStatus = case orderPI.status of
        SPI.INSTOCK -> SRideBooking.NEW
        SPI.CONFIRMED -> SRideBooking.CONFIRMED
        SPI.TRIP_ASSIGNED -> SRideBooking.TRIP_ASSIGNED
        SPI.COMPLETED -> SRideBooking.COMPLETED
        SPI.CANCELLED -> SRideBooking.CANCELLED
        _ -> SRideBooking.TRIP_ASSIGNED
  mbRide <-
    -- COMPLETED and TRIP_ASSIGNED means that there IS ride. But CANCELLED does not, it could be cancelled
    -- before TRIP_ASSIGNED. So technically there is a bug until we change our storage model.
    if rbStatus `elem` [SRideBooking.COMPLETED, SRideBooking.TRIP_ASSIGNED, SRideBooking.CANCELLED]
      then Just <$> buildRideAPIEntity orderPI
      else return Nothing

  return $
    API.RideBookingStatusRes
      { id = orderPI.id,
        status = rbStatus,
        agencyName = org.name,
        agencyNumber = org.mobileCountryCode <> org.mobileNumber,
        estimatedPrice = orderPI.price,
        toLocation = SLoc.makeSearchReqLocationAPIEntity toLocation,
        fromLocation = SLoc.makeSearchReqLocationAPIEntity fromLocation,
        ride = mbRide,
        createdAt = orderPI.createdAt,
        updatedAt = orderPI.updatedAt
      }

buildRideAPIEntity :: DBFlow m r => SPI.ProductInstance -> m SRide.RideAPIEntity
buildRideAPIEntity orderPI = do
  let rideStatus = case orderPI.status of
        SPI.INPROGRESS -> SRide.INPROGRESS
        SPI.COMPLETED -> SRide.COMPLETED
        SPI.CANCELLED -> SRide.CANCELLED
        _ -> SRide.NEW
  info :: Info.ProductInfo <- orderPI.info >>= decodeFromText & fromMaybeM (InternalError "Unable to read product info.")
  return $
    SRide.RideAPIEntity
      { id = orderPI.id,
        shortRideId = orderPI.shortId,
        status = rideStatus,
        driverName = info.tracker >>= (.trip.driver) <&> (.name),
        driverNumber = info.tracker >>= (.trip.driver) <&> (.phones) >>= listToMaybe,
        driverRatings = info.tracker >>= (.trip.driver) >>= (.rating) <&> (.value) >>= readMaybe . T.unpack,
        driverRegisteredAt = info.tracker >>= (.trip.driver) <&> (.registeredAt),
        vehicleNumber = info.tracker >>= (.trip.vehicle) >>= (.registrationNumber),
        vehicleColor = info.tracker >>= (.trip.vehicle) >>= (.color),
        vehicleVariant = info.tracker >>= (.trip.vehicle) <&> (.variant),
        vehicleModel = info.tracker >>= (.trip.vehicle) >>= (.model),
        rideOtp = orderPI.udf4,
        computedPrice = orderPI.price,
        actualRideDistance = orderPI.actualDistance,
        createdAt = orderPI.createdAt,
        updatedAt = orderPI.updatedAt
      }
