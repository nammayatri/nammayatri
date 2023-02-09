module Domain.Action.Beckn.OnInit where

import Domain.Types.Booking (BPPBooking, Booking)
import qualified Domain.Types.Booking as DRB
import qualified Domain.Types.LocationAddress as DBL
import Kernel.External.Encryption (decrypt)
import Kernel.Prelude
import qualified Kernel.Storage.Esqueleto as DB
import Kernel.Types.Id
import Kernel.Utils.Common
import Kernel.Utils.GenericPretty (PrettyShow)
import Storage.CachedQueries.CacheConfig
import qualified Storage.CachedQueries.Merchant as QMerch
import qualified Storage.Queries.Booking as QRideB
import qualified Storage.Queries.Person as QP
import Tools.Error

data OnInitReq = OnInitReq
  { bookingId :: Id Booking,
    bppBookingId :: Id BPPBooking,
    estimatedFare :: Money,
    discount :: Maybe Money,
    estimatedTotalFare :: Money
  }
  deriving (Show, Generic, FromJSON, ToJSON, ToSchema)

data OnInitRes = OnInitRes
  { bookingId :: Id DRB.Booking,
    bppBookingId :: Id DRB.BPPBooking,
    bppId :: Text,
    bppUrl :: BaseUrl,
    fromLocationAddress :: DBL.LocationAddress,
    mbToLocationAddress :: Maybe DBL.LocationAddress,
    estimatedTotalFare :: Money,
    riderPhoneCountryCode :: Text,
    riderPhoneNumber :: Text,
    mbRiderName :: Maybe Text
  }
  deriving (Generic, Show, PrettyShow)

onInit :: (CacheFlow m r, EsqDBFlow m r, EncFlow m r) => BaseUrl -> OnInitReq -> m OnInitRes
onInit registryUrl req = do
  bookingOld <- QRideB.findById req.bookingId >>= fromMaybeM (BookingDoesNotExist req.bookingId.getId)

  -- TODO: this supposed to be temporary solution. Check if we still need it
  merchant <- QMerch.findById bookingOld.merchantId >>= fromMaybeM (MerchantNotFound bookingOld.merchantId.getId)
  unless (merchant.registryUrl == registryUrl) $ throwError (InvalidRequest "Merchant doesnt't work with passed url.")

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
        DRB.DriverOfferDetails details -> Just details.toLocation
  return $
    OnInitRes
      { bookingId = booking.id,
        bppId = booking.providerId,
        bppUrl = booking.providerUrl,
        estimatedTotalFare = booking.estimatedTotalFare,
        fromLocationAddress = fromLocation.address,
        mbToLocationAddress = mbToLocation <&> (.address),
        mbRiderName = decRider.firstName,
        ..
      }
