module Product.Booking where

import App.Types
import Beckn.Types.Id
import qualified Domain.Types.Booking as SRB
import qualified Domain.Types.Booking.BookingLocation as SLoc
import qualified Domain.Types.FareBreakup as DFareBreakup
import qualified Domain.Types.Person as Person
import qualified Domain.Types.RentalSlab as DRentalSlab
import qualified Domain.Types.Ride as SRide
import EulerHS.Prelude hiding (id)
import qualified Storage.Queries.Booking as QRB
import qualified Storage.Queries.FareBreakup as QFareBreakup
import qualified Storage.Queries.Ride as QRide
import qualified Types.API.Booking as API
import Types.Error
import Utils.Common

bookingStatus :: Id SRB.Booking -> Id Person.Person -> FlowHandler API.BookingStatusRes
bookingStatus bookingId personId = withFlowHandlerAPI $ do
  booking <- QRB.findById bookingId >>= fromMaybeM (BookingDoesNotExist bookingId.getId)
  unless (booking.riderId == personId) $ throwError AccessDenied
  buildBookingStatusRes booking

bookingList :: Id Person.Person -> Maybe Integer -> Maybe Integer -> Maybe Bool -> FlowHandler API.BookingListRes
bookingList personId mbLimit mbOffset mbOnlyActive = withFlowHandlerAPI $ do
  rbList <- QRB.findAllByRiderIdAndRide personId mbLimit mbOffset mbOnlyActive
  API.BookingListRes <$> traverse buildBookingStatusRes rbList

buildBookingStatusRes :: EsqDBFlow m r => SRB.Booking -> m API.BookingStatusRes
buildBookingStatusRes booking = do
  ride <- QRide.findActiveByRBId booking.id >>= fromMaybeM (RideDoesNotExist (booking.id).getId)
  rideAPIEntityList <-
    QRide.findAllByRBId booking.id
      <&> fmap SRide.makeRideAPIEntity
  fareBreakups <- QFareBreakup.findAllByBookingId booking.id
  let bookingDetails = mkBookingAPIDetails booking.bookingDetails
  let timeTimeInMinutes =
        if isNothing (ride.rideStartTime) || isNothing (ride.rideEndTime)
          then Nothing
          else do
            startTime <- ride.rideStartTime
            endTime <- ride.rideEndTime
            Just (nominalDiffTimeToSeconds (diffUTCTime endTime startTime))
  return $
    API.BookingStatusRes
      { id = booking.id,
        status = booking.status,
        agencyName = booking.providerName,
        agencyNumber = booking.providerMobileNumber,
        estimatedFare = booking.estimatedFare,
        discount = booking.discount,
        estimatedTotalFare = booking.estimatedTotalFare,
        fromLocation = SLoc.makeBookingLocationAPIEntity booking.fromLocation,
        rideList = rideAPIEntityList,
        tripTerms = fromMaybe [] $ booking.tripTerms <&> (.descriptions),
        fareBreakup = DFareBreakup.mkFareBreakupAPIEntity <$> fareBreakups,
        bookingDetails,
        rideStartTime = ride.rideStartTime,
        rideEndTime = ride.rideEndTime,
        duration = timeTimeInMinutes,
        createdAt = booking.createdAt,
        updatedAt = booking.updatedAt
      }

mkBookingAPIDetails :: SRB.BookingDetails -> API.BookingAPIDetails
mkBookingAPIDetails = \case
  SRB.OneWayDetails details -> API.OneWayAPIDetails . mkOneWayAPIDetails $ details
  SRB.RentalDetails DRentalSlab.RentalSlab {..} -> API.RentalAPIDetails DRentalSlab.RentalSlabAPIEntity {..}
  SRB.DriverOfferDetails details -> API.DriverOfferAPIDetails . mkOneWayAPIDetails $ details
  where
    mkOneWayAPIDetails SRB.OneWayBookingDetails {..} =
      API.OneWayBookingAPIDetails
        { toLocation = SLoc.makeBookingLocationAPIEntity toLocation
        }
