{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wwarn=incomplete-record-updates #-}

module Domain.Action.Beckn.OnStatus
  ( onStatus,
    DOnStatusReq (..),
    RideDetails (..),
    OnStatusFareBreakup (..),
    NewRideInfo (..),
    RideStartedInfo (..),
    RideCompletedInfo (..),
  )
where

import qualified Data.HashMap.Strict as HM
import qualified Domain.Action.Beckn.Common as DCommon
import qualified Domain.Types.Booking as DB
import qualified Domain.Types.BookingCancellationReason as DBCR
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Ride as DRide
import EulerHS.Prelude hiding (id)
import Kernel.Beam.Functions as B
import Kernel.Sms.Config (SmsConfig)
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import Kernel.Types.Common hiding (id)
import Kernel.Types.Error
import Kernel.Types.Id (Id)
import Kernel.Utils.Common
import Lib.SessionizerMetrics.Types.Event
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.Queries.Booking as QB
import qualified Storage.Queries.BookingCancellationReason as QBCR
import qualified Storage.Queries.Ride as QRide
import Tools.Metrics (HasBAPMetrics)

data DOnStatusReq = DOnStatusReq
  { bppBookingId :: Id DB.BPPBooking,
    rideDetails :: RideDetails
  }

data NewRideInfo = NewRideInfo
  { bppRideId :: Id DRide.BPPRide,
    driverName :: Text,
    driverImage :: Maybe Text,
    driverMobileNumber :: Text,
    driverMobileCountryCode :: Maybe Text,
    driverRating :: Maybe Centesimal,
    driverRegisteredAt :: UTCTime,
    otp :: Text,
    vehicleNumber :: Text,
    vehicleColor :: Text,
    vehicleModel :: Text
  }

data RideStartedInfo = RideStartedInfo
  { rideStartTime :: UTCTime,
    driverArrivalTime :: Maybe UTCTime
  }

data RideCompletedInfo = RideCompletedInfo
  { rideEndTime :: UTCTime,
    fare :: Money,
    totalFare :: Money,
    fareBreakups :: [OnStatusFareBreakup],
    chargeableDistance :: HighPrecMeters,
    traveledDistance :: HighPrecMeters,
    paymentUrl :: Maybe Text
  }

data RideDetails
  = NewBookingDetails
  | RideAssignedDetails DCommon.RideAssignedReq
  | RideStartedDetails DCommon.RideStartedReq
  | RideCompletedDetails DCommon.RideCompletedReq
  | BookingCancelledDetails DCommon.BookingCancelledReq
  | BookingReallocationDetails
      { bookingDetails :: DCommon.BookingDetails,
        reallocationSource :: DBCR.CancellationSource
      }
  | DriverArrivedDetails DCommon.DriverArrivedReq

-- the same as OnUpdateFareBreakup
data OnStatusFareBreakup = OnStatusFareBreakup
  { amount :: HighPrecMoney,
    description :: Text
  }

data RideEntity
  = UpdatedRide
      { ride :: DRide.Ride,
        rideOldStatus :: DRide.RideStatus
      }
  | RenewedRide
      { ride :: DRide.Ride
      }

buildRideEntity :: KvDbFlow m r => DB.Booking -> (DRide.Ride -> DRide.Ride) -> DCommon.BookingDetails -> m RideEntity
buildRideEntity booking updRide newRideInfo = do
  mbExistingRide <- B.runInReplica $ QRide.findByBPPRideId newRideInfo.bppRideId
  case mbExistingRide of
    Nothing -> do
      mbMerchant <- CQM.findById booking.merchantId
      newRide <- buildNewRide mbMerchant booking newRideInfo
      pure $ RenewedRide (updRide newRide)
    Just existingRide -> do
      unless (existingRide.bookingId == booking.id) $ throwError (InvalidRequest "Invalid rideId")
      pure $ UpdatedRide {ride = updRide existingRide, rideOldStatus = existingRide.status}

rideBookingTransaction :: KvDbFlow m r => DB.BookingStatus -> DRide.RideStatus -> DB.Booking -> RideEntity -> m ()
rideBookingTransaction bookingNewStatus rideNewStatus booking rideEntity = do
  unless (booking.status == bookingNewStatus) $ do
    QB.updateStatus booking.id bookingNewStatus
  case rideEntity of
    UpdatedRide {ride, rideOldStatus} -> do
      unless (rideOldStatus == rideNewStatus) $ do
        QRide.updateMultiple ride.id ride
    RenewedRide renewedRide -> do
      QRide.create renewedRide

isStatusChanged :: DB.BookingStatus -> DB.BookingStatus -> RideEntity -> Bool
isStatusChanged bookingOldStatus bookingNewStatus rideEntity = do
  let bookingStatusChanged = bookingOldStatus == bookingNewStatus
  let rideStatusChanged = case rideEntity of
        UpdatedRide {ride, rideOldStatus} -> rideOldStatus == ride.status
        RenewedRide {} -> True
  bookingStatusChanged || rideStatusChanged

onStatus ::
  ( MonadFlow m,
    HasField "minTripDistanceForReferralCfg" r (Maybe HighPrecMeters),
    KvDbFlow m r,
    HasBAPMetrics m r,
    EncFlow m r,
    HasHttpClientOptions r c,
    HasLongDurationRetryCfg r c,
    EventStreamFlow m r,
    EsqDBReplicaFlow m r,
    HasFlowEnv m r '["internalEndPointHashMap" ::: HM.HashMap BaseUrl BaseUrl],
    HasFlowEnv m r '["nwAddress" ::: BaseUrl, "smsCfg" ::: SmsConfig],
    HasField "hotSpotExpiry" r Seconds
  ) =>
  DOnStatusReq ->
  m ()
onStatus req = do
  booking <- QB.findByBPPBookingId req.bppBookingId >>= fromMaybeM (BookingDoesNotExist $ "BppBookingId: " <> req.bppBookingId.getId)
  case req.rideDetails of
    NewBookingDetails -> do
      mbExistingRide <- B.runInReplica $ QRide.findActiveByRBId booking.id
      unless (booking.status == bookingNewStatus) $ do
        QB.updateStatus booking.id bookingNewStatus
      whenJust mbExistingRide \existingRide -> do
        unless (existingRide.status == rideNewStatus) $ do
          QRide.updateStatus existingRide.id rideNewStatus
      where
        bookingNewStatus = DB.NEW
        rideNewStatus = DRide.CANCELLED
    RideAssignedDetails request -> DCommon.rideAssignedReqHandler request
    DriverArrivedDetails request -> DCommon.driverArrivedReqHandler request
    RideStartedDetails request -> DCommon.rideStartedReqHandler request
    RideCompletedDetails request -> DCommon.rideCompletedReqHandler request
    BookingCancelledDetails request -> DCommon.bookingCancelledReqHandler request
    BookingReallocationDetails {bookingDetails, reallocationSource} -> do
      rideEntity <- buildRideEntity booking updateReallocatedRide bookingDetails
      let rideId = case rideEntity of
            UpdatedRide {ride} -> ride.id
            RenewedRide {ride} -> ride.id
      let bookingCancellationReason = mkBookingCancellationReason booking.id (Just rideId) reallocationSource booking.merchantId
      rideBookingTransaction bookingNewStatus rideNewStatus booking rideEntity
      when (isStatusChanged booking.status bookingNewStatus rideEntity) $ do
        QBCR.upsert bookingCancellationReason
      where
        bookingNewStatus = DB.AWAITING_REASSIGNMENT
        rideNewStatus = DRide.CANCELLED
        updateReallocatedRide newRide = newRide{status = rideNewStatus}

buildNewRide :: MonadFlow m => Maybe DM.Merchant -> DB.Booking -> DCommon.BookingDetails -> m DRide.Ride
buildNewRide mbMerchant booking DCommon.BookingDetails {..} = do
  id <- generateGUID
  shortId <- generateShortId
  now <- getCurrentTime
  let fromLocation = booking.fromLocation
      toLocation = case booking.bookingDetails of
        DB.OneWayDetails details -> Just details.toLocation
        DB.RentalDetails _ -> Nothing
        DB.DriverOfferDetails details -> Just details.toLocation
        DB.OneWaySpecialZoneDetails details -> Just details.toLocation
        DB.InterCityDetails details -> Just details.toLocation
  let allowedEditLocationAttempts = Just $ maybe 0 (.numOfAllowedEditPickupLocationAttemptsThreshold) mbMerchant
  let createdAt = now
      updatedAt = now
      merchantId = Just booking.merchantId
      merchantOperatingCityId = Just booking.merchantOperatingCityId
      bookingId = booking.id
      status = DRide.NEW
      vehicleVariant = booking.vehicleVariant
      trackingUrl = Nothing
      fare = Nothing
      totalFare = Nothing
      chargeableDistance = Nothing
      traveledDistance = Nothing
      driverArrivalTime = Nothing
      rideStartTime = Nothing
      rideEndTime = Nothing
      rideRating = Nothing
      isFreeRide = Nothing
      safetyCheckStatus = Nothing
      endOtp = Nothing
      startOdometerReading = Nothing
      endOdometerReading = Nothing
  pure $ DRide.Ride {..}

mkBookingCancellationReason ::
  Id DB.Booking ->
  Maybe (Id DRide.Ride) ->
  DBCR.CancellationSource ->
  Id DM.Merchant ->
  DBCR.BookingCancellationReason
mkBookingCancellationReason bookingId mbRideId cancellationSource merchantId = do
  DBCR.BookingCancellationReason
    { bookingId = bookingId,
      rideId = mbRideId,
      merchantId = Just merchantId,
      source = cancellationSource,
      reasonCode = Nothing,
      reasonStage = Nothing,
      additionalInfo = Nothing,
      driverCancellationLocation = Nothing,
      driverDistToPickup = Nothing
    }
