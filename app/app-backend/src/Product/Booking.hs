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
  rideAPIEntityList <-
    QRide.findAllByRBId booking.id
      <&> fmap SRide.makeRideAPIEntity
  fareBreakups <- QFareBreakup.findAllByBookingId booking.id
  let bookingDetails = mkBookingAPIDetails booking.bookingDetails

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
        createdAt = booking.createdAt,
        updatedAt = booking.updatedAt
      }

mkBookingAPIDetails :: SRB.BookingDetails -> API.BookingAPIDetails
mkBookingAPIDetails = \case
  SRB.OneWayDetails SRB.OneWayBookingDetails {..} ->
    API.OneWayAPIDetails
      API.OneWayBookingAPIDetails
        { toLocation = SLoc.makeBookingLocationAPIEntity toLocation
        }
  SRB.RentalDetails DRentalSlab.RentalSlab {..} ->
    API.RentalAPIDetails DRentalSlab.RentalSlabAPIEntity {..}
