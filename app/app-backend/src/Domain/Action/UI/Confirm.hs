module Domain.Action.UI.Confirm
  ( confirm,
    ConfirmReq (..),
    ConfirmRes (..),
  )
where

import Beckn.External.Encryption
import Beckn.Prelude
import qualified Beckn.Storage.Esqueleto as DB
import Beckn.Types.Amount
import Beckn.Types.Common hiding (id)
import Beckn.Types.Id
import qualified Domain.Types.BookingLocation as DBL
import qualified Domain.Types.Person as Person
import qualified Domain.Types.RideBooking as DRB
import qualified Storage.Queries.BookingLocation as QBL
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.RideBooking as QRB
import Types.Error
import Utils.Common

data ConfirmReq = ConfirmReq
  { bookingId :: Id DRB.RideBooking,
    fromLocation :: DBL.BookingLocationAPIEntity,
    toLocation :: Maybe DBL.BookingLocationAPIEntity
  }
  deriving (Show, Generic, FromJSON, ToJSON, ToSchema)

data ConfirmRes = ConfirmRes
  { bppBookingId :: Id DRB.BPPRideBooking,
    bppId :: Text,
    bppUrl :: BaseUrl,
    fromLocationAddress :: DBL.LocationAddress,
    toLocationAddress :: Maybe DBL.LocationAddress,
    estimatedTotalFare :: Amount,
    riderPhoneCountryCode :: Text,
    riderPhoneNumber :: Text
  }

confirm :: (EncFlow m r, EsqDBFlow m r) => Id Person.Person -> ConfirmReq -> m ConfirmRes
confirm personId req = do
  booking <- QRB.findById req.bookingId >>= fromMaybeM SearchRequestDoesNotExist
  unless (booking.riderId == personId) $ throwError AccessDenied
  decRider <- QP.findById booking.riderId >>= fromMaybeM PersonNotFound >>= decrypt
  riderPhoneCountryCode <- decRider.mobileCountryCode & fromMaybeM (PersonFieldNotPresent "mobileCountryCode")
  riderPhoneNumber <- decRider.mobileNumber & fromMaybeM (PersonFieldNotPresent "mobileNumber")
  bppBookingId <- booking.bppBookingId & fromMaybeM (RideBookingFieldNotPresent "bppBookingId")
  let fromLocationAddress = makeLocationAddressFromAPIEntity req.fromLocation
      toLocationAddress = makeLocationAddressFromAPIEntity <$> req.toLocation
  DB.runTransaction $ do
    QBL.updateAddress booking.fromLocationId fromLocationAddress
    whenJust toLocationAddress $ QBL.updateAddress booking.fromLocationId
  return $
    ConfirmRes
      { bppId = booking.providerId,
        bppUrl = booking.providerUrl,
        estimatedTotalFare = booking.estimatedTotalFare,
        ..
      }

makeLocationAddressFromAPIEntity :: DBL.BookingLocationAPIEntity -> DBL.LocationAddress
makeLocationAddressFromAPIEntity DBL.BookingLocationAPIEntity {..} =
  DBL.LocationAddress {..}
