{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.Beckn.OnInit where

import Domain.Types.Booking (BPPBooking, Booking)
import qualified Domain.Types.Booking as DRB
import qualified Domain.Types.Location as DL
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.VehicleVariant as Veh
import Kernel.External.Encryption (decrypt)
import Kernel.Prelude
import Kernel.Storage.Hedis (HedisFlow)
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.Queries.Booking as QRideB
import qualified Storage.Queries.Person as QP
import Tools.Error

data OnInitReq = OnInitReq
  { bookingId :: Id Booking,
    bppBookingId :: Id BPPBooking,
    estimatedFare :: Money,
    discount :: Maybe Money,
    estimatedTotalFare :: Money,
    paymentUrl :: Maybe Text
  }
  deriving (Show, Generic, FromJSON, ToJSON, ToSchema)

data OnInitRes = OnInitRes
  { bookingId :: Id DRB.Booking,
    bppBookingId :: Id DRB.BPPBooking,
    bookingDetails :: DRB.BookingDetails,
    driverId :: Maybe Text,
    paymentUrl :: Maybe Text,
    vehicleVariant :: Veh.VehicleVariant,
    itemId :: Text,
    fulfillmentId :: Maybe Text,
    bppId :: Text,
    bppUrl :: BaseUrl,
    fromLocation :: DL.Location,
    mbToLocation :: Maybe DL.Location,
    estimatedTotalFare :: Money,
    estimatedFare :: Money,
    riderPhoneCountryCode :: Text,
    riderPhoneNumber :: Text,
    mbRiderName :: Maybe Text,
    transactionId :: Text,
    merchant :: DM.Merchant
  }
  deriving (Generic, Show)

onInit :: (CacheFlow m r, EsqDBFlow m r, EncFlow m r, HedisFlow m r) => OnInitReq -> m OnInitRes
onInit req = do
  void $ QRideB.updateBPPBookingId req.bookingId req.bppBookingId
  void $ QRideB.updatePaymentInfo req.bookingId req.estimatedFare req.discount req.estimatedTotalFare req.paymentUrl
  booking <- QRideB.findById req.bookingId >>= fromMaybeM (BookingDoesNotExist req.bookingId.getId)
  merchant <- CQM.findById booking.merchantId >>= fromMaybeM (MerchantNotFound booking.merchantId.getId)
  decRider <- QP.findById booking.riderId >>= fromMaybeM (PersonNotFound booking.riderId.getId) >>= decrypt
  riderPhoneCountryCode <- decRider.mobileCountryCode & fromMaybeM (PersonFieldNotPresent "mobileCountryCode")
  riderPhoneNumber <- decRider.mobileNumber & fromMaybeM (PersonFieldNotPresent "mobileNumber")
  bppBookingId <- booking.bppBookingId & fromMaybeM (BookingFieldNotPresent "bppBookingId")
  let fromLocation = booking.fromLocation
  let mbToLocation = case booking.bookingDetails of
        DRB.RentalDetails _ _ -> Nothing
        DRB.OneWayDetails details -> Just details.toLocation
        DRB.DriverOfferDetails details -> Just details.toLocation
        DRB.OneWaySpecialZoneDetails details -> Just details.toLocation
  return $
    OnInitRes
      { bookingId = booking.id,
        driverId = booking.driverId,
        paymentUrl = booking.paymentUrl,
        itemId = booking.itemId,
        vehicleVariant = booking.vehicleVariant,
        fulfillmentId = booking.fulfillmentId,
        bookingDetails = booking.bookingDetails,
        bppId = booking.providerId,
        bppUrl = booking.providerUrl,
        estimatedTotalFare = booking.estimatedTotalFare,
        estimatedFare = booking.estimatedFare,
        fromLocation = fromLocation,
        mbToLocation = mbToLocation,
        mbRiderName = decRider.firstName,
        transactionId = booking.transactionId,
        merchant = merchant,
        ..
      }
