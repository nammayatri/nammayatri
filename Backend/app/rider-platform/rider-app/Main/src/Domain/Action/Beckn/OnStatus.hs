{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.Beckn.OnStatus
  ( onStatus,
    DOnStatusReq (..),
    RideDetails (..),
    OnStatusFareBreakup (..),
    NewRideInfo (..),
    RideStartedInfo (..),
    RideCompletedInfo (..),
    ValidatedOnStatusReq (..),
    BookingReallocationReq (..),
    validateRequest,
  )
where

import qualified Data.HashMap.Strict as HM
import qualified Domain.Action.Beckn.Common as DCommon
import qualified Domain.Types.Booking as DB
import qualified Domain.Types.BookingCancellationReason as DBCR
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Ride as DRide
import qualified Domain.Types.VehicleVariant as DV
import EulerHS.Prelude hiding (id)
import Kernel.Beam.Functions as B
import Kernel.External.Encryption
import Kernel.External.Types (SchedulerFlow)
import Kernel.Sms.Config (SmsConfig)
import Kernel.Storage.Clickhouse.Config
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
import TransactionLogs.Types
import qualified UrlShortner.Common as UrlShortner

data DOnStatusReq = DOnStatusReq
  { bppBookingId :: Id DB.BPPBooking,
    rideDetails :: RideDetails
  }

data ValidatedOnStatusReq = ValidatedOnStatusReq
  { bppBookingId :: Id DB.BPPBooking,
    validatedRideDetails :: ValidatedRideDetails
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
  | RideEnroutePickupDetails
  | RideStartedDetails DCommon.RideStartedReq
  | RideCompletedDetails DCommon.RideCompletedReq
  | BookingCancelledDetails DCommon.BookingCancelledReq
  | BookingReallocationDetails BookingReallocationReq
  | DriverArrivedDetails DCommon.DriverArrivedReq

data ValidatedRideDetails
  = ValidatedNewBookingDetails
  | ValidatedRideAssignedDetails DCommon.ValidatedRideAssignedReq
  | ValidatedRideEnroutePickupDetails
  | ValidatedRideStartedDetails DCommon.ValidatedRideStartedReq
  | ValidatedRideCompletedDetails DCommon.ValidatedRideCompletedReq
  | ValidatedFarePaidDetails DCommon.ValidatedFarePaidReq
  | ValidatedBookingCancelledDetails DCommon.ValidatedBookingCancelledReq
  | ValidatedBookingReallocationDetails ValidatedBookingReallocationReq
  | ValidatedDriverArrivedDetails DCommon.ValidatedDriverArrivedReq

data BookingReallocationReq = BookingReallocationReq
  { bookingDetails :: DCommon.BookingDetails,
    reallocationSource :: DBCR.CancellationSource
  }

type ValidatedBookingReallocationReq = BookingReallocationReq

-- the same as OnUpdateFareBreakup
data OnStatusFareBreakup = OnStatusFareBreakup
  { amount :: HighPrecMoney,
    description :: Text
  }

data RideEntity = UpdatedRide DUpdatedRide | RenewedRide DRide.Ride

data DUpdatedRide = DUpdatedRide
  { ride :: DRide.Ride,
    rideOldStatus :: DRide.RideStatus
  }

buildRideEntity :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r, EncFlow m r) => DB.Booking -> (DRide.Ride -> DRide.Ride) -> DCommon.BookingDetails -> m RideEntity
buildRideEntity booking updRide newRideInfo = do
  mbExistingRide <- B.runInReplica $ QRide.findByBPPRideId newRideInfo.bppRideId
  case mbExistingRide of
    Nothing -> do
      mbMerchant <- CQM.findById booking.merchantId
      newRide <- buildNewRide mbMerchant booking newRideInfo
      pure $ RenewedRide (updRide newRide)
    Just existingRide -> do
      unless (existingRide.bookingId == booking.id) $ throwError (InvalidRequest "Invalid rideId")
      pure $ UpdatedRide $ DUpdatedRide {ride = updRide existingRide, rideOldStatus = existingRide.status}

rideBookingTransaction :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r, HasField "storeRidesTimeLimit" r Int) => DB.BookingStatus -> DRide.RideStatus -> DB.Booking -> RideEntity -> m ()
rideBookingTransaction bookingNewStatus rideNewStatus booking rideEntity = do
  unless (booking.status == bookingNewStatus) $ do
    QB.updateStatus booking.id bookingNewStatus
  -- not making booking parties link inactive as this function is not used
  case rideEntity of
    UpdatedRide (DUpdatedRide {ride, rideOldStatus}) -> do
      unless (rideOldStatus == rideNewStatus) $ do
        QRide.updateMultiple ride.id ride
    RenewedRide renewedRide -> do
      QRide.createRide renewedRide

isStatusChanged :: DB.BookingStatus -> DB.BookingStatus -> RideEntity -> Bool
isStatusChanged bookingOldStatus bookingNewStatus rideEntity = do
  let bookingStatusChanged = bookingOldStatus == bookingNewStatus
  let rideStatusChanged = case rideEntity of
        UpdatedRide (DUpdatedRide {ride, rideOldStatus}) -> rideOldStatus == ride.status
        RenewedRide {} -> True
  bookingStatusChanged || rideStatusChanged

-- TODO: When making a onStatus request, make sure status only goes in forward direction.
onStatus ::
  ( MonadFlow m,
    HasField "minTripDistanceForReferralCfg" r (Maybe Distance),
    CacheFlow m r,
    EsqDBFlow m r,
    ClickhouseFlow m r,
    HasBAPMetrics m r,
    EncFlow m r,
    SchedulerFlow r,
    HasHttpClientOptions r c,
    HasLongDurationRetryCfg r c,
    HasShortDurationRetryCfg r c,
    EventStreamFlow m r,
    EsqDBReplicaFlow m r,
    HasFlowEnv m r '["internalEndPointHashMap" ::: HM.HashMap BaseUrl BaseUrl],
    HasFlowEnv m r '["nwAddress" ::: BaseUrl, "smsCfg" ::: SmsConfig],
    HasFlowEnv m r '["ondcTokenHashMap" ::: HM.HashMap KeyConfig TokenConfig],
    HasField "storeRidesTimeLimit" r Int,
    HasField "hotSpotExpiry" r Seconds,
    HasFlowEnv m r '["urlShortnerConfig" ::: UrlShortner.UrlShortnerConfig]
  ) =>
  ValidatedOnStatusReq ->
  m ()
onStatus req = do
  booking <- QB.findByBPPBookingId req.bppBookingId >>= fromMaybeM (BookingDoesNotExist $ "BppBookingId: " <> req.bppBookingId.getId)
  case req.validatedRideDetails of
    ValidatedNewBookingDetails -> do
      mbExistingRide <- B.runInReplica $ QRide.findActiveByRBId booking.id
      unless (booking.status == bookingNewStatus) $ do
        QB.updateStatus booking.id bookingNewStatus
      -- not making booking parties link inactive as this function is not used
      whenJust mbExistingRide \existingRide -> do
        unless (existingRide.status == rideNewStatus) $ do
          QRide.updateStatus existingRide.id rideNewStatus
      where
        bookingNewStatus = DB.NEW
        rideNewStatus = DRide.CANCELLED
    ValidatedRideAssignedDetails request -> DCommon.rideAssignedReqHandler request
    ValidatedRideEnroutePickupDetails -> do
      logTagInfo "OnStatus" "RIDE_ENROUTE_PICKUP event received"
      pure ()
    ValidatedDriverArrivedDetails request -> DCommon.driverArrivedReqHandler request
    ValidatedRideStartedDetails request -> DCommon.rideStartedReqHandler request
    ValidatedRideCompletedDetails request -> DCommon.rideCompletedReqHandler request
    ValidatedFarePaidDetails request -> DCommon.farePaidReqHandler request
    ValidatedBookingCancelledDetails request -> DCommon.bookingCancelledReqHandler request
    ValidatedBookingReallocationDetails BookingReallocationReq {bookingDetails, reallocationSource} -> do
      rideEntity <- buildRideEntity booking updateReallocatedRide bookingDetails
      let rideId = case rideEntity of
            UpdatedRide (DUpdatedRide {ride}) -> ride.id
            RenewedRide ride -> ride.id
      bookingCancellationReason <- mkBookingCancellationReason booking (Just rideId) reallocationSource
      rideBookingTransaction bookingNewStatus rideNewStatus booking rideEntity
      when (isStatusChanged booking.status bookingNewStatus rideEntity) $ do
        QBCR.upsert bookingCancellationReason
      where
        bookingNewStatus = DB.AWAITING_REASSIGNMENT
        rideNewStatus = DRide.CANCELLED
        updateReallocatedRide newRide = newRide{status = rideNewStatus}

validateRequest ::
  ( MonadFlow m,
    HasField "minTripDistanceForReferralCfg" r (Maybe Distance),
    CacheFlow m r,
    EsqDBFlow m r,
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
  m ValidatedOnStatusReq
validateRequest req@DOnStatusReq {..} = do
  case req.rideDetails of
    NewBookingDetails -> do
      let validatedRideDetails = ValidatedNewBookingDetails
      return ValidatedOnStatusReq {..}
    RideAssignedDetails request -> do
      rideDetails' <- DCommon.validateRideAssignedReq request
      let validatedRideDetails = ValidatedRideAssignedDetails rideDetails'
      return ValidatedOnStatusReq {..}
    RideEnroutePickupDetails -> do
      let validatedRideDetails = ValidatedRideEnroutePickupDetails
      return ValidatedOnStatusReq {..}
    DriverArrivedDetails request -> do
      rideDetails' <- DCommon.validateDriverArrivedReq request
      let validatedRideDetails = ValidatedDriverArrivedDetails rideDetails'
      return ValidatedOnStatusReq {..}
    RideStartedDetails request -> do
      rideDetails' <- DCommon.validateRideStartedReq request
      let validatedRideDetails = ValidatedRideStartedDetails rideDetails'
      return ValidatedOnStatusReq {..}
    RideCompletedDetails request -> do
      rideDetails' <- DCommon.validateRideCompletedReq request
      case rideDetails' of
        Left rideDetails'' -> do
          let validatedRideDetails = ValidatedRideCompletedDetails rideDetails''
          return ValidatedOnStatusReq {..}
        Right rideDetails'' -> do
          let validatedRideDetails = ValidatedFarePaidDetails rideDetails''
          return ValidatedOnStatusReq {..}
    BookingCancelledDetails request -> do
      rideDetails' <- DCommon.validateBookingCancelledReq request
      let validatedRideDetails = ValidatedBookingCancelledDetails rideDetails'
      return ValidatedOnStatusReq {..}
    BookingReallocationDetails request -> do
      let validatedRideDetails = ValidatedBookingReallocationDetails request
      return ValidatedOnStatusReq {..}

buildNewRide :: (MonadFlow m, EncFlow m r) => Maybe DM.Merchant -> DB.Booking -> DCommon.BookingDetails -> m DRide.Ride
buildNewRide mbMerchant booking DCommon.BookingDetails {..} = do
  id <- generateGUID
  shortId <- generateShortId
  now <- getCurrentTime
  let fromLocation = booking.fromLocation
      (toLocation, stops) = case booking.bookingDetails of
        DB.OneWayDetails details -> (Just details.toLocation, details.stops)
        DB.RentalDetails _ -> (Nothing, [])
        DB.DriverOfferDetails details -> (Just details.toLocation, details.stops)
        DB.OneWaySpecialZoneDetails details -> (Just details.toLocation, details.stops)
        DB.InterCityDetails details -> (Just details.toLocation, [])
        DB.AmbulanceDetails details -> (Just details.toLocation, [])
        DB.DeliveryDetails details -> (Just details.toLocation, [])
  let allowedEditLocationAttempts = Just $ maybe 0 (.numOfAllowedEditLocationAttemptsThreshold) mbMerchant
  let allowedEditPickupLocationAttempts = Just $ maybe 0 (.numOfAllowedEditPickupLocationAttemptsThreshold) mbMerchant
  driverPhoneNumber' <- encrypt driverMobileNumber
  driverAlternateNumber' <- mapM encrypt driverAlternatePhoneNumber
  let createdAt = now
      updatedAt = now
      merchantId = Just booking.merchantId
      merchantOperatingCityId = Just booking.merchantOperatingCityId
      bookingId = booking.id
      status = DRide.NEW
      vehicleVariant = DV.castServiceTierToVariant booking.vehicleServiceTierType
      vehicleServiceTierType = Just booking.vehicleServiceTierType
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
      driversPreviousRideDropLoc = Nothing
      showDriversPreviousRideDropLoc = False
      clientId = booking.clientId
      backendAppVersion = Nothing
      backendConfigVersion = Nothing
      clientBundleVersion = Nothing
      clientConfigVersion = Nothing
      clientDevice = Nothing
      clientSdkVersion = Nothing
      tollConfidence = Nothing
      distanceUnit = booking.distanceUnit
      paymentStatus = DRide.NotInitiated
      driverAccountId = Nothing
      vehicleAge = Nothing
      driverPhoneNumber = Just driverPhoneNumber'
      driverAlternateNumber = driverAlternateNumber'
      onlinePayment = maybe False (.onlinePayment) mbMerchant
      cancellationFeeIfCancelled = Nothing
      isAlreadyFav = Just False
      favCount = Just 0
      safetyJourneyStatus = Nothing
      destinationReachedAt = Nothing
      estimatedEndTimeRange = Nothing
      tipAmount = Nothing
      hasStops = booking.hasStops
      wasRideSafe = Nothing
      feedbackSkipped = False
      pickupRouteCallCount = Just 0
      talkedWithDriver = Nothing
  pure $ DRide.Ride {..}

mkBookingCancellationReason ::
  (MonadFlow m) =>
  DB.Booking ->
  Maybe (Id DRide.Ride) ->
  DBCR.CancellationSource ->
  m DBCR.BookingCancellationReason
mkBookingCancellationReason booking mbRideId cancellationSource = do
  now <- getCurrentTime
  return $
    DBCR.BookingCancellationReason
      { bookingId = booking.id,
        rideId = mbRideId,
        merchantId = Just booking.merchantId,
        distanceUnit = booking.distanceUnit,
        source = cancellationSource,
        reasonCode = Nothing,
        reasonStage = Nothing,
        additionalInfo = Nothing,
        driverCancellationLocation = Nothing,
        driverDistToPickup = Nothing,
        riderId = Just booking.riderId,
        createdAt = now,
        updatedAt = now
      }
