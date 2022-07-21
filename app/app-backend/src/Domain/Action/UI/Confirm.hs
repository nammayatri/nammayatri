module Domain.Action.UI.Confirm
  ( confirm,
    ConfirmReq (..),
    ConfirmLocationReq (..),
    ConfirmRes (..),
  )
where

import Beckn.External.Encryption
import Beckn.Prelude
import qualified Beckn.Storage.Esqueleto as DB
import Beckn.Types.Amount
import Beckn.Types.Common hiding (id)
import Beckn.Types.Id
import Beckn.Utils.GenericPretty
import qualified Domain.Types.Booking as DRB
import qualified Domain.Types.BookingLocation as DBL
import qualified Domain.Types.Person as Person
import qualified Storage.Queries.Booking as QRB
import qualified Storage.Queries.BookingLocation as QBL
import qualified Storage.Queries.Person as QP
import Types.Error
import Utils.Common

data ConfirmReq = ConfirmReq
  { bookingId :: Id DRB.Booking,
    fromLocation :: ConfirmLocationReq,
    toLocation :: Maybe ConfirmLocationReq
  }
  deriving (Show, Generic, FromJSON, ToJSON, ToSchema)

data ConfirmLocationReq = ConfirmLocationReq
  { street :: Maybe Text,
    door :: Maybe Text,
    city :: Maybe Text,
    state :: Maybe Text,
    country :: Maybe Text,
    building :: Maybe Text,
    areaCode :: Maybe Text,
    area :: Maybe Text
  }
  deriving (Show, Generic, FromJSON, ToJSON, ToSchema)

data ConfirmRes = ConfirmRes
  { bppBookingId :: Id DRB.BPPBooking,
    bppId :: Text,
    bppUrl :: BaseUrl,
    fromLocationAddress :: DBL.LocationAddress,
    toLocationAddress :: Maybe DBL.LocationAddress,
    estimatedTotalFare :: Amount,
    riderPhoneCountryCode :: Text,
    riderPhoneNumber :: Text
  }
  deriving (Generic, Show, PrettyShow)

confirm :: (EncFlow m r, EsqDBFlow m r) => Id Person.Person -> ConfirmReq -> m ConfirmRes
confirm personId req = do
  booking <- QRB.findById req.bookingId >>= fromMaybeM (BookingDoesNotExist req.bookingId.getId)
  unless (booking.riderId == personId) $ throwError AccessDenied
  decRider <- QP.findById booking.riderId >>= fromMaybeM (PersonNotFound booking.riderId.getId) >>= decrypt
  riderPhoneCountryCode <- decRider.mobileCountryCode & fromMaybeM (PersonFieldNotPresent "mobileCountryCode")
  riderPhoneNumber <- decRider.mobileNumber & fromMaybeM (PersonFieldNotPresent "mobileNumber")
  bppBookingId <- booking.bppBookingId & fromMaybeM (BookingFieldNotPresent "bppBookingId")
  let fromLocationAddress = makeLocationAddress req.fromLocation
      toLocationAddress = makeLocationAddress <$> req.toLocation
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

makeLocationAddress :: ConfirmLocationReq -> DBL.LocationAddress
makeLocationAddress ConfirmLocationReq {..} =
  DBL.LocationAddress {..}
