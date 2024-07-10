{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.UI.Feedback
  ( FeedbackReq (..),
    FeedbackRes (..),
    DriverProfileResponse (..),
    feedback,
    knowYourFavDriver,
    knowYourDriver,
  )
where

import qualified Domain.Action.Internal.Rating as DRating
import qualified Domain.Types.Booking as DBooking
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Person as Person
import qualified Domain.Types.PersonFlowStatus as DPFS
import qualified Domain.Types.Ride as DRide
import qualified Domain.Types.VehicleServiceTier as DVST
import qualified Domain.Types.VehicleVariant as DVeh
import qualified Environment as App
import Kernel.External.Encryption (decrypt)
import Kernel.Prelude
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified SharedLogic.CallBPPInternal as CallBPPInternal
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.CachedQueries.Person.PersonFlowStatus as QPFS
import qualified Storage.CachedQueries.ValueAddNP as CQVAN
import qualified Storage.Queries.Booking as QRB
import qualified Storage.Queries.Issue as QIssue
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.Rating as QRating
import qualified Storage.Queries.Ride as QRide
import Tools.Error

data FeedbackReq = FeedbackReq
  { rideId :: Id DRide.Ride,
    rating :: Int,
    feedbackDetails :: Maybe Text,
    nightSafety :: Maybe Bool,
    shouldFavDriver :: Maybe Bool,
    wasOfferedAssistance :: Maybe Bool
  }
  deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

data FeedbackRes = FeedbackRes
  { bppBookingId :: Id DBooking.BPPBooking,
    ratingValue :: Int,
    feedbackDetails :: Maybe Text,
    providerId :: Text,
    providerUrl :: BaseUrl,
    transactionId :: Text,
    merchant :: DM.Merchant,
    wasOfferedAssistance :: Maybe Bool,
    city :: Context.City,
    issueId :: Maybe Text,
    vehicleVariant :: DVeh.VehicleVariant,
    vehicleServiceTierType :: DVST.VehicleServiceTierType,
    shouldFavDriver :: Maybe Bool,
    riderPhoneNum :: Maybe Text,
    isValueAddNP :: Bool
  }

data DriverProfileResponse = DriverProfileResponse
  { response :: Maybe CallBPPInternal.DriverProfileRes
  }
  deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

feedback :: FeedbackReq -> Id Person.Person -> App.Flow FeedbackRes
feedback request personId = do
  let ratingValue = request.rating
  unless (ratingValue `elem` [1 .. 5]) $ throwError InvalidRatingValue
  let rideId = request.rideId
      feedbackDetails = request.feedbackDetails
  ride <- QRide.findById rideId >>= fromMaybeM (RideDoesNotExist rideId.getId)
  unless (ride.status == DRide.COMPLETED) $ throwError (RideInvalidStatus "Feedback available only for completed rides.")
  booking <- QRB.findById ride.bookingId >>= fromMaybeM (BookingNotFound ride.bookingId.getId)
  merchant <- CQM.findById booking.merchantId >>= fromMaybeM (MerchantNotFound booking.merchantId.getId)
  bppBookingId <- booking.bppBookingId & fromMaybeM (BookingFieldNotPresent "bppBookingId")
  issueId' <- getIssueIdForRide booking
  _ <- QPFS.updateStatus booking.riderId DPFS.IDLE
  _ <- QRide.updateRideRating rideId ratingValue
  ratingu <- QRating.findRatingForRide rideId
  _ <- case ratingu of
    Nothing -> do
      newRating <- DRating.buildRating rideId booking.riderId ratingValue feedbackDetails request.wasOfferedAssistance
      QRating.create newRating
    Just rideRating -> do
      QRating.updateRating ratingValue feedbackDetails request.wasOfferedAssistance rideRating.id booking.riderId
  let merchantOperatingCityId = booking.merchantOperatingCityId
  city <- CQMOC.findById merchantOperatingCityId >>= fmap (.city) . fromMaybeM (MerchantOperatingCityNotFound merchantOperatingCityId.getId)
  person <- QPerson.findById personId >>= fromMaybeM (PersonDoesNotExist personId.getId)
  isValueAddNP <- CQVAN.isValueAddNP booking.providerId
  unencryptedMobileNumber <- mapM decrypt person.mobileNumber
  pure
    FeedbackRes
      { wasOfferedAssistance = request.wasOfferedAssistance,
        providerId = booking.providerId,
        providerUrl = booking.providerUrl,
        transactionId = booking.transactionId,
        issueId = issueId',
        vehicleVariant = DVST.castServiceTierToVariant booking.vehicleServiceTierType,
        vehicleServiceTierType = booking.vehicleServiceTierType,
        shouldFavDriver = request.shouldFavDriver,
        riderPhoneNum = unencryptedMobileNumber,
        ..
      }
  where
    getIssueIdForRide booking = do
      res <- QIssue.findNightIssueByBookingId booking.id
      case res of
        Just issue -> return $ Just issue.id.getId
        Nothing -> return Nothing

knowYourDriver :: Id DRide.Ride -> App.Flow DriverProfileResponse
knowYourDriver rideId = do
  ride <- QRide.findById rideId >>= fromMaybeM (RideDoesNotExist rideId.getId)
  unless (ride.status == DRide.COMPLETED) $ throwError (RideInvalidStatus "KnowYourDriver available only for completed rides.")
  booking <- QRB.findById ride.bookingId >>= fromMaybeM (BookingNotFound ride.bookingId.getId)
  merchant <- CQM.findById booking.merchantId >>= fromMaybeM (MerchantNotFound booking.merchantId.getId)
  isValueAddNP <- CQVAN.isValueAddNP booking.providerId
  if isValueAddNP
    then do
      res <- CallBPPInternal.getknowYourDriverDetails merchant.driverOfferApiKey merchant.driverOfferBaseUrl ride.bppRideId.getId
      pure $ DriverProfileResponse {response = Just res}
    else pure $ DriverProfileResponse {response = Nothing}

knowYourFavDriver :: Text -> Id DM.Merchant -> App.Flow DriverProfileResponse
knowYourFavDriver driverId merchantId = do
  merchant <- CQM.findById merchantId >>= fromMaybeM (MerchantNotFound merchantId.getId)
  res <- CallBPPInternal.getKnowYourFavDriverDetails merchant.driverOfferApiKey merchant.driverOfferBaseUrl driverId
  pure $ DriverProfileResponse {response = Just res}
