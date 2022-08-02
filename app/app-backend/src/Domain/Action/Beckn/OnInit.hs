module Domain.Action.Beckn.OnInit where

import Beckn.External.Encryption (decrypt)
import Beckn.Prelude
import qualified Beckn.Storage.Esqueleto as DB
import Beckn.Types.Amount
import Beckn.Types.Id
import Beckn.Utils.GenericPretty (PrettyShow)
import Domain.Types.Booking (BPPBooking, Booking)
import qualified Domain.Types.Booking as DRB
import qualified Domain.Types.Booking.BookingLocation as DBL
import qualified Storage.Queries.Booking as QRideB
import qualified Storage.Queries.Person as QP
import Types.Error
import Utils.Common

data OnInitReq = OnInitReq
  { bookingId :: Id Booking,
    bppBookingId :: Id BPPBooking,
    estimatedFare :: Amount,
    discount :: Maybe Amount,
    estimatedTotalFare :: Amount
  }
  deriving (Show, Generic, FromJSON, ToJSON, ToSchema)

data OnInitRes = OnInitRes
  { bookingId :: Id DRB.Booking,
    bppBookingId :: Id DRB.BPPBooking,
    bppId :: Text,
    bppUrl :: BaseUrl,
    fromLocationAddress :: DBL.LocationAddress,
    mbToLocationAddress :: Maybe DBL.LocationAddress,
    estimatedTotalFare :: Amount,
    riderPhoneCountryCode :: Text,
    riderPhoneNumber :: Text
  }
  deriving (Generic, Show, PrettyShow)

onInit :: (EsqDBFlow m r, EncFlow m r) => OnInitReq -> m OnInitRes
onInit req = do
  DB.runTransaction $ do
    QRideB.updateBPPBookingId req.bookingId req.bppBookingId
    QRideB.updatePaymentInfo req.bookingId req.estimatedFare req.discount req.estimatedTotalFare
  booking <- QRideB.findById req.bookingId >>= fromMaybeM (BookingDoesNotExist req.bookingId.getId)
  decRider <- QP.findById booking.riderId >>= fromMaybeM (PersonNotFound booking.riderId.getId) >>= decrypt
  riderPhoneCountryCode <- decRider.mobileCountryCode & fromMaybeM (PersonFieldNotPresent "mobileCountryCode")
  riderPhoneNumber <- decRider.mobileNumber & fromMaybeM (PersonFieldNotPresent "mobileNumber")
  bppBookingId <- booking.bppBookingId & fromMaybeM (BookingFieldNotPresent "bppBookingId")
  let fromLocation = booking.fromLocation
  let mbToLocation = case booking.bookingDetails of
        DRB.RentalDetails _ -> Nothing
        DRB.OneWayDetails details -> Just details.toLocation
  return $
    OnInitRes
      { bookingId = booking.id,
        bppId = booking.providerId,
        bppUrl = booking.providerUrl,
        estimatedTotalFare = booking.estimatedTotalFare,
        fromLocationAddress = fromLocation.address,
        mbToLocationAddress = mbToLocation <&> (.address),
        ..
      }
