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
import Domain.Types.Person as Person
import qualified Domain.Types.VehicleServiceTier as DVST
import qualified Domain.Types.VehicleVariant as Veh
import Kernel.External.Encryption (decrypt)
import Kernel.Prelude
import Kernel.Storage.Hedis (HedisFlow)
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.CachedQueries.Merchant.RiderConfig as CQRC
import qualified Storage.CachedQueries.ValueAddNP as CQVAN
import qualified Storage.Queries.Booking as QRideB
import qualified Storage.Queries.Person as QP
import Tools.Error
import Tools.Notifications

data OnInitReq = OnInitReq
  { bookingId :: Id Booking,
    bppBookingId :: Maybe (Id BPPBooking),
    estimatedFare :: Price,
    discount :: Maybe Price,
    -- estimatedTotalFare :: Price,
    paymentUrl :: Maybe Text,
    paymentId :: Maybe Text
  }
  deriving (Show, Generic)

data OnInitRes = OnInitRes
  { bookingId :: Id DRB.Booking,
    bppBookingId :: Maybe (Id DRB.BPPBooking),
    bookingDetails :: DRB.BookingDetails,
    paymentUrl :: Maybe Text,
    vehicleVariant :: Veh.VehicleVariant,
    itemId :: Text,
    fulfillmentId :: Maybe Text,
    bppId :: Text,
    bppUrl :: BaseUrl,
    fromLocation :: DL.Location,
    mbToLocation :: Maybe DL.Location,
    estimatedTotalFare :: Price,
    estimatedFare :: Price,
    riderPhoneCountryCode :: Text,
    riderPhoneNumber :: Text,
    mbRiderName :: Maybe Text,
    transactionId :: Text,
    merchant :: DM.Merchant,
    city :: Context.City,
    nightSafetyCheck :: Bool,
    isValueAddNP :: Bool,
    enableFrequentLocationUpdates :: Bool,
    paymentId :: Maybe Text
  }
  deriving (Generic, Show)

onInit :: (KvDbFlow m r, EncFlow m r, HedisFlow m r) => OnInitReq -> m (OnInitRes, DRB.Booking)
onInit req = do
  whenJust req.bppBookingId $ QRideB.updateBPPBookingId req.bookingId
  void $ QRideB.updatePaymentInfo req.bookingId req.estimatedFare req.discount req.estimatedFare req.paymentUrl -- TODO : 4th parameter is discounted fare (not implemented)
  booking <- QRideB.findById req.bookingId >>= fromMaybeM (BookingDoesNotExist req.bookingId.getId)
  merchant <- CQM.findById booking.merchantId >>= fromMaybeM (MerchantNotFound booking.merchantId.getId)
  decRider <- QP.findById booking.riderId >>= fromMaybeM (PersonNotFound booking.riderId.getId) >>= decrypt
  isValueAddNP <- CQVAN.isValueAddNP booking.providerId
  riderPhoneCountryCode <- decRider.mobileCountryCode & fromMaybeM (PersonFieldNotPresent "mobileCountryCode")
  riderPhoneNumber <-
    if isValueAddNP
      then decRider.mobileNumber & fromMaybeM (PersonFieldNotPresent "mobileNumber")
      else pure $ prependZero booking.primaryExophone
  let bppBookingId = booking.bppBookingId
  city <-
    CQMOC.findById booking.merchantOperatingCityId
      >>= fmap (.city) . fromMaybeM (MerchantOperatingCityNotFound booking.merchantOperatingCityId.getId)
  riderConfig <- CQRC.findByMerchantOperatingCityId decRider.merchantOperatingCityId >>= fromMaybeM (RiderConfigDoesNotExist decRider.merchantOperatingCityId.getId)
  now <- getLocalCurrentTime riderConfig.timeDiffFromUtc
  let fromLocation = booking.fromLocation
  let mbToLocation = case booking.bookingDetails of
        DRB.RentalDetails _ -> Nothing
        DRB.OneWayDetails details -> Just details.toLocation
        DRB.DriverOfferDetails details -> Just details.toLocation
        DRB.OneWaySpecialZoneDetails details -> Just details.toLocation
        DRB.InterCityDetails details -> Just details.toLocation
  let onInitRes =
        OnInitRes
          { bookingId = booking.id,
            paymentUrl = booking.paymentUrl,
            itemId = booking.itemId,
            vehicleVariant = DVST.castServiceTierToVariant booking.vehicleServiceTierType,
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
            nightSafetyCheck = decRider.nightSafetyChecks,
            enableFrequentLocationUpdates =
              decRider.shareTripWithEmergencyContactOption == Just ALWAYS_SHARE
                || ( decRider.shareTripWithEmergencyContactOption == Just SHARE_WITH_TIME_CONSTRAINTS
                       && checkTimeConstraintForFollowRide riderConfig now
                   ),
            paymentId = req.paymentId,
            ..
          }
  pure (onInitRes, booking)
  where
    prependZero :: Text -> Text
    prependZero str = "0" <> str
