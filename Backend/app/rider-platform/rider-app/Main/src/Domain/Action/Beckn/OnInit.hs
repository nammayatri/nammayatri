{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.Beckn.OnInit where

import Domain.Types
import Domain.Types.Booking (BPPBooking, Booking)
import qualified Domain.Types.Booking as DRB
import qualified Domain.Types.Location as DL
import qualified Domain.Types.Merchant as DM
import Domain.Types.Person as Person
import qualified Domain.Types.VehicleVariant as DV
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
import Storage.Queries.SafetySettings as QSafety
import Tools.Error
import qualified Tools.Metrics as Metrics
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
    vehicleVariant :: DV.VehicleVariant,
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
    paymentId :: Maybe Text,
    enableOtpLessRide :: Bool,
    tripCategory :: Maybe TripCategory
  }
  deriving (Generic, Show)

onInit :: (CacheFlow m r, EsqDBFlow m r, EncFlow m r, HedisFlow m r, Metrics.HasBAPMetrics m r) => OnInitReq -> m (OnInitRes, DRB.Booking)
onInit req = do
  whenJust req.bppBookingId $ QRideB.updateBPPBookingId req.bookingId
  void $ QRideB.updatePaymentInfo req.bookingId req.estimatedFare req.discount req.estimatedFare req.paymentUrl -- TODO : 4th parameter is discounted fare (not implemented)
  booking <- QRideB.findById req.bookingId >>= fromMaybeM (BookingDoesNotExist req.bookingId.getId)
  merchant <- CQM.findById booking.merchantId >>= fromMaybeM (MerchantNotFound booking.merchantId.getId)
  person <- QP.findById booking.riderId >>= fromMaybeM (PersonNotFound booking.riderId.getId)
  decRider <- decrypt person
  safetySettings <- QSafety.findSafetySettingsWithFallback booking.riderId (Just person)
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
      mbToLocation = getToLocationFromBookingDetails booking.bookingDetails
      onInitRes =
        OnInitRes
          { bookingId = booking.id,
            paymentUrl = booking.paymentUrl,
            itemId = booking.bppEstimateId,
            vehicleVariant = DV.castServiceTierToVariant booking.vehicleServiceTierType,
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
            nightSafetyCheck = checkSafetySettingConstraint (Just safetySettings.enableUnexpectedEventsCheck) riderConfig now,
            enableFrequentLocationUpdates = checkSafetySettingConstraint safetySettings.aggregatedRideShareSetting riderConfig now,
            paymentId = req.paymentId,
            enableOtpLessRide = fromMaybe False safetySettings.enableOtpLessRide,
            tripCategory = booking.tripCategory,
            ..
          }
  Metrics.finishMetricsBap Metrics.INIT merchant.name booking.transactionId booking.merchantOperatingCityId.getId
  pure (onInitRes, booking)
  where
    prependZero :: Text -> Text
    prependZero str = "0" <> str

    checkSafetySettingConstraint item riderConfig now =
      case item of
        Just ALWAYS_SHARE -> True
        Just SHARE_WITH_TIME_CONSTRAINTS -> checkTimeConstraintForFollowRide riderConfig now
        _ -> False

    getToLocationFromBookingDetails = \case
      DRB.RentalDetails _ -> Nothing
      DRB.OneWayDetails details -> Just details.toLocation
      DRB.DriverOfferDetails details -> Just details.toLocation
      DRB.OneWaySpecialZoneDetails details -> Just details.toLocation
      DRB.InterCityDetails details -> Just details.toLocation
      DRB.AmbulanceDetails details -> Just details.toLocation
      DRB.DeliveryDetails details -> Just details.toLocation
      DRB.OneWayScheduledDetails details -> Just details.toLocation
