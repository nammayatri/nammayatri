module Product.RideBooking where

import App.Types
import Beckn.Types.Id
import qualified Domain.Types.BookingLocation as SLoc
import qualified Domain.Types.FareBreakup as DFareBreakup
import qualified Domain.Types.Person as Person
import qualified Domain.Types.Ride as SRide
import qualified Domain.Types.RideBooking as SRB
import EulerHS.Prelude hiding (id)
import qualified Storage.Queries.BookingLocation as QLoc
import qualified Storage.Queries.FareBreakup as QFareBreakup
import qualified Storage.Queries.Ride as QRide
import qualified Storage.Queries.RideBooking as QRB
import qualified Types.API.RideBooking as API
import Types.Error
import Utils.Common

rideBookingStatus :: Id SRB.RideBooking -> Id Person.Person -> FlowHandler API.RideBookingStatusRes
rideBookingStatus rideBookingId personId = withFlowHandlerAPI $ do
  rideBooking <- QRB.findById rideBookingId >>= fromMaybeM (RideBookingDoesNotExist rideBookingId.getId)
  unless (rideBooking.riderId == personId) $ throwError AccessDenied
  buildRideBookingStatusRes rideBooking

rideBookingList :: Id Person.Person -> Maybe Integer -> Maybe Integer -> Maybe Bool -> FlowHandler API.RideBookingListRes
rideBookingList personId mbLimit mbOffset mbOnlyActive = withFlowHandlerAPI $ do
  rbList <- QRB.findAllByRiderIdAndRide personId mbLimit mbOffset mbOnlyActive
  API.RideBookingListRes <$> traverse buildRideBookingStatusRes rbList

buildRideBookingStatusRes :: EsqDBFlow m r => SRB.RideBooking -> m API.RideBookingStatusRes
buildRideBookingStatusRes rideBooking = do
  fromLocation <- QLoc.findById rideBooking.fromLocationId >>= fromMaybeM LocationNotFound
  (mbToLocation, baseDistance, baseDuration) <- case rideBooking.rideBookingDetails of
    SRB.OneWayDetails details -> do
      toLocation <- QLoc.findById details.toLocationId >>= fromMaybeM LocationNotFound
      pure (Just toLocation, Nothing, Nothing)
    SRB.RentalDetails details -> do
      pure (Nothing, Just details.baseDistance, Just details.baseDuration)
  rideAPIEntityList <-
    QRide.findAllByRBId rideBooking.id
      <&> fmap SRide.makeRideAPIEntity
  fareBreakups <- QFareBreakup.findAllByRideBookingId rideBooking.id

  return $
    API.RideBookingStatusRes
      { id = rideBooking.id,
        status = rideBooking.status,
        agencyName = rideBooking.providerName,
        agencyNumber = rideBooking.providerMobileNumber,
        estimatedFare = rideBooking.estimatedFare,
        discount = rideBooking.discount,
        estimatedTotalFare = rideBooking.estimatedTotalFare,
        toLocation = SLoc.makeBookingLocationAPIEntity <$> mbToLocation,
        fromLocation = SLoc.makeBookingLocationAPIEntity fromLocation,
        rideList = rideAPIEntityList,
        tripTerms = fromMaybe [] $ rideBooking.tripTerms <&> (.descriptions),
        fareBreakup = DFareBreakup.mkFareBreakupAPIEntity <$> fareBreakups,
        createdAt = rideBooking.createdAt,
        updatedAt = rideBooking.updatedAt,
        baseDistance,
        baseDuration
      }
