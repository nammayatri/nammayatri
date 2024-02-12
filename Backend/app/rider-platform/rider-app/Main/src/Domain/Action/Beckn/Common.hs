{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wwarn=incomplete-record-updates #-}

module Domain.Action.Beckn.Common where

import qualified Data.HashMap.Strict as HM
import Data.Time hiding (getCurrentTime)
import Domain.Action.UI.HotSpot
import qualified Domain.Types.Booking as DRB
import qualified Domain.Types.BookingCancellationReason as DBCR
-- import qualified Domain.Types.Estimate as DEstimate
import qualified Domain.Types.FarePolicy.FareBreakup as DFareBreakup
import Domain.Types.HotSpot
import qualified Domain.Types.Merchant as DMerchant
-- import qualified Domain.Types.Person as DP
import qualified Domain.Types.Person.PersonFlowStatus as DPFS
-- import Domain.Types.Ride
import qualified Domain.Types.Ride as DRide
-- import qualified Domain.Types.SearchRequest as DSR
-- import Domain.Types.VehicleVariant
-- import Environment
import Kernel.Beam.Functions
import qualified Kernel.External.Maps as Maps
import Kernel.Prelude
import Kernel.Sms.Config (SmsConfig)
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.SessionizerMetrics.Types.Event
import qualified SharedLogic.CallBPP as CallBPP
import qualified SharedLogic.MerchantConfig as SMC
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.CachedQueries.MerchantConfig as CMC
import qualified Storage.CachedQueries.Person.PersonFlowStatus as QPFS
import qualified Storage.Queries.Booking as QRB
import qualified Storage.Queries.BookingCancellationReason as QBCR
-- import qualified Storage.Queries.Estimate as QEstimate
import qualified Storage.Queries.FareBreakup as QFareBreakup
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.Ride as QRide
-- import qualified Storage.Queries.SearchRequest as QSR
import Tools.Error
import Tools.Event
import Tools.Maps (LatLong)
import Tools.Metrics (HasBAPMetrics, incrementRideCreatedRequestCount)
import qualified Tools.Notifications as Notify

data BookingDetails = BookingDetails
  { bppBookingId :: Id DRB.BPPBooking,
    bppRideId :: Id DRide.BPPRide,
    driverName :: Text,
    driverImage :: Maybe Text,
    driverMobileNumber :: Text,
    driverMobileCountryCode :: Maybe Text,
    driverRating :: Maybe Centesimal,
    driverRegisteredAt :: Maybe UTCTime,
    vehicleNumber :: Text,
    vehicleColor :: Text,
    vehicleModel :: Text,
    otp :: Text
  }

data RideAssignedReq = RideAssignedReq
  { bookingDetails :: BookingDetails,
    isDriverBirthDay :: Bool,
    isFreeRide :: Bool
  }

data RideStartedReq = RideStartedReq
  { bookingDetails :: BookingDetails,
    tripStartLocation :: Maybe LatLong,
    endOtp_ :: Maybe Text,
    startOdometerReading :: Maybe Centesimal,
    rideStartTime :: Maybe UTCTime,
    driverArrivalTime :: Maybe UTCTime
  }

data RideCompletedReq = RideCompletedReq
  { bookingDetails :: BookingDetails,
    fare :: Money,
    totalFare :: Money,
    fareBreakups :: [DFareBreakup],
    chargeableDistance :: Maybe HighPrecMeters,
    traveledDistance :: Maybe HighPrecMeters,
    paymentUrl :: Maybe Text,
    tripEndLocation :: Maybe LatLong,
    endOdometerReading :: Maybe Centesimal,
    rideEndTime :: Maybe UTCTime
  }

data BookingCancelledReq = BookingCancelledReq
  { bookingDetails :: Maybe BookingDetails,
    bppBookingId :: Id DRB.BPPBooking,
    cancellationSource :: DBCR.CancellationSource
  }

data BookingReallocationReq = BookingReallocationReq ----need to use in future
  { bookingDetails :: BookingDetails,
    reallocationSource :: DBCR.CancellationSource
  }

data DriverArrivedReq = DriverArrivedReq
  { bookingDetails :: BookingDetails,
    arrivalTime :: Maybe UTCTime
  }

data DFareBreakup = DFareBreakup
  { amount :: HighPrecMoney,
    description :: Text
  }

rideAssignedReqHandler ::
  ( HasFlowEnv m r '["nwAddress" ::: BaseUrl, "smsCfg" ::: SmsConfig],
    CacheFlow m r,
    EsqDBFlow m r,
    MonadFlow m,
    EncFlow m r,
    EsqDBReplicaFlow m r,
    HasHttpClientOptions r c,
    HasLongDurationRetryCfg r c,
    -- HasShortDurationRetryCfg r c, -- uncomment for test update api
    HasField "minTripDistanceForReferralCfg" r (Maybe HighPrecMeters),
    HasFlowEnv m r '["internalEndPointHashMap" ::: HM.HashMap BaseUrl BaseUrl],
    -- HasFlowEnv m r '["isBecknSpecVersion2" ::: Bool],
    HasBAPMetrics m r,
    EventStreamFlow m r,
    HasField "hotSpotExpiry" r Seconds
  ) =>
  RideAssignedReq ->
  m ()
rideAssignedReqHandler RideAssignedReq {..} = do
  let BookingDetails {..} = bookingDetails
  booking <- runInReplica $ QRB.findByBPPBookingId bppBookingId >>= fromMaybeM (BookingDoesNotExist $ "BppBookingId: " <> bppBookingId.getId)
  unless (isAssignable booking) $ throwError (BookingInvalidStatus $ show booking.status)
  mbMerchant <- CQM.findById booking.merchantId
  ride <- buildRide mbMerchant booking bookingDetails
  triggerRideCreatedEvent RideEventData {ride = ride, personId = booking.riderId, merchantId = booking.merchantId}
  let category = case booking.specialLocationTag of
        Just _ -> "specialLocation"
        Nothing -> "normal"
  incrementRideCreatedRequestCount booking.merchantId.getId booking.merchantOperatingCityId.getId category
  _ <- QRB.updateStatus booking.id DRB.TRIP_ASSIGNED
  _ <- QRide.createRide ride

  _ <- QPFS.updateStatus booking.riderId DPFS.RIDE_PICKUP {rideId = ride.id, bookingId = booking.id, trackingUrl = Nothing, otp, vehicleNumber, fromLocation = Maps.getCoordinates booking.fromLocation, driverLocation = Nothing}
  QPFS.clearCache booking.riderId
  Notify.notifyOnRideAssigned booking ride
  when isDriverBirthDay $ do
    Notify.notifyDriverBirthDay booking.riderId driverName
  withLongRetry $ CallBPP.callTrack booking ride
  where
    buildRide :: MonadFlow m => Maybe DMerchant.Merchant -> DRB.Booking -> BookingDetails -> m DRide.Ride
    buildRide mbMerchant booking BookingDetails {..} = do
      guid <- generateGUID
      shortId <- generateShortId
      now <- getCurrentTime
      let fromLocation = booking.fromLocation
          toLocation = case booking.bookingDetails of
            DRB.OneWayDetails details -> Just details.toLocation
            DRB.RentalDetails _ -> Nothing
            DRB.DriverOfferDetails details -> Just details.toLocation
            DRB.OneWaySpecialZoneDetails details -> Just details.toLocation
            DRB.InterCityDetails details -> Just details.toLocation
      let allowedEditLocationAttempts = Just $ maybe 0 (.numOfAllowedEditPickupLocationAttemptsThreshold) mbMerchant
      return
        DRide.Ride
          { id = guid,
            bookingId = booking.id,
            merchantId = Just booking.merchantId,
            merchantOperatingCityId = Just booking.merchantOperatingCityId,
            status = DRide.NEW,
            trackingUrl = Nothing,
            fare = Nothing,
            totalFare = Nothing,
            chargeableDistance = Nothing,
            traveledDistance = Nothing,
            driverArrivalTime = Nothing,
            vehicleVariant = booking.vehicleVariant,
            createdAt = now,
            updatedAt = now,
            rideStartTime = Nothing,
            rideEndTime = Nothing,
            rideRating = Nothing,
            safetyCheckStatus = Nothing,
            isFreeRide = Just isFreeRide,
            endOtp = Nothing,
            startOdometerReading = Nothing,
            endOdometerReading = Nothing,
            ..
          }

    isAssignable booking = booking.status `elem` [DRB.CONFIRMED, DRB.AWAITING_REASSIGNMENT]

rideStartedReqHandler ::
  ( HasFlowEnv m r '["nwAddress" ::: BaseUrl, "smsCfg" ::: SmsConfig],
    CacheFlow m r,
    EsqDBFlow m r,
    MonadFlow m,
    EncFlow m r,
    EsqDBReplicaFlow m r,
    HasHttpClientOptions r c,
    HasLongDurationRetryCfg r c,
    -- HasShortDurationRetryCfg r c, -- uncomment for test update api
    HasField "minTripDistanceForReferralCfg" r (Maybe HighPrecMeters),
    HasFlowEnv m r '["internalEndPointHashMap" ::: HM.HashMap BaseUrl BaseUrl],
    -- HasFlowEnv m r '["isBecknSpecVersion2" ::: Bool],
    HasBAPMetrics m r,
    EventStreamFlow m r,
    HasField "hotSpotExpiry" r Seconds
  ) =>
  RideStartedReq ->
  m ()
rideStartedReqHandler RideStartedReq {..} = do
  let BookingDetails {..} = bookingDetails
  booking <- runInReplica $ QRB.findByBPPBookingId bookingDetails.bppBookingId >>= fromMaybeM (BookingDoesNotExist $ "BppBookingId: " <> bookingDetails.bppBookingId.getId)
  ride <- QRide.findByBPPRideId bppRideId >>= fromMaybeM (RideDoesNotExist $ "BppRideId" <> bppRideId.getId)
  unless (booking.status == DRB.TRIP_ASSIGNED) $ throwError (BookingInvalidStatus $ show booking.status)
  unless (ride.status == DRide.NEW) $ throwError (RideInvalidStatus $ show ride.status)

  fork "ride start geohash frequencyUpdater" $ do
    case tripStartLocation of
      Just location -> frequencyUpdator booking.merchantId location Nothing TripStart
      Nothing -> return ()
  let updRideForStartReq =
        ride{status = DRide.INPROGRESS,
             rideStartTime,
             rideEndTime = Nothing,
             endOtp = endOtp_,
             driverArrivalTime,
             startOdometerReading
            }
  triggerRideStartedEvent RideEventData {ride = updRideForStartReq, personId = booking.riderId, merchantId = booking.merchantId}
  _ <- QRide.updateMultiple updRideForStartReq.id updRideForStartReq
  _ <- QPFS.updateStatus booking.riderId DPFS.RIDE_STARTED {rideId = ride.id, bookingId = booking.id, trackingUrl = ride.trackingUrl, driverLocation = Nothing}
  QPFS.clearCache booking.riderId
  fork "notify emergency contacts" $ Notify.notifyRideStartToEmergencyContacts booking ride
  Notify.notifyOnRideStarted booking ride

rideCompletedReqHandler ::
  ( HasFlowEnv m r '["nwAddress" ::: BaseUrl, "smsCfg" ::: SmsConfig],
    CacheFlow m r,
    EsqDBFlow m r,
    MonadFlow m,
    EncFlow m r,
    EsqDBReplicaFlow m r,
    HasHttpClientOptions r c,
    HasLongDurationRetryCfg r c,
    -- HasShortDurationRetryCfg r c, -- uncomment for test update api
    HasField "minTripDistanceForReferralCfg" r (Maybe HighPrecMeters),
    HasFlowEnv m r '["internalEndPointHashMap" ::: HM.HashMap BaseUrl BaseUrl],
    -- HasFlowEnv m r '["isBecknSpecVersion2" ::: Bool],
    HasBAPMetrics m r,
    EventStreamFlow m r,
    HasField "hotSpotExpiry" r Seconds
  ) =>
  RideCompletedReq ->
  m ()
rideCompletedReqHandler RideCompletedReq {..} = do
  let BookingDetails {..} = bookingDetails
  booking <- runInReplica $ QRB.findByBPPBookingId bookingDetails.bppBookingId >>= fromMaybeM (BookingDoesNotExist $ "BppBookingId: " <> bookingDetails.bppBookingId.getId)
  ride <- QRide.findByBPPRideId bppRideId >>= fromMaybeM (RideDoesNotExist $ "BppRideId" <> bppRideId.getId)
  let bookingCanBeCompleted = booking.status == DRB.TRIP_ASSIGNED
      rideCanBeCompleted = ride.status == DRide.INPROGRESS
      bookingAlreadyCompleted = booking.status == DRB.COMPLETED
      rideAlreadyCompleted = ride.status == DRide.COMPLETED
  unless (bookingCanBeCompleted || (bookingAlreadyCompleted && rideCanBeCompleted)) $
    throwError (BookingInvalidStatus $ show booking.status)
  unless (rideCanBeCompleted || (rideAlreadyCompleted && bookingCanBeCompleted)) $
    throwError (RideInvalidStatus $ show ride.status)
  person <- QP.findById booking.riderId >>= fromMaybeM (PersonNotFound booking.riderId.getId)

  fork "ride end geohash frequencyUpdater" $ do
    case tripEndLocation of
      Just location -> frequencyUpdator booking.merchantId location Nothing TripEnd
      Nothing -> return ()
  SMC.updateTotalRidesCounters booking.riderId
  merchantConfigs <- CMC.findAllByMerchantOperatingCityId booking.merchantOperatingCityId
  SMC.updateTotalRidesInWindowCounters booking.riderId merchantConfigs

  let updRide =
        ride{status = DRide.COMPLETED,
             fare = Just fare,
             totalFare = Just totalFare,
             chargeableDistance,
             rideEndTime,
             endOdometerReading
            }
  breakups <- traverse (buildFareBreakup booking.id) fareBreakups
  minTripDistanceForReferralCfg <- asks (.minTripDistanceForReferralCfg)
  let shouldUpdateRideComplete =
        case minTripDistanceForReferralCfg of
          Just distance -> updRide.chargeableDistance >= Just distance && not person.hasTakenValidRide
          Nothing -> True
  triggerRideEndEvent RideEventData {ride = updRide, personId = booking.riderId, merchantId = booking.merchantId}
  triggerBookingCompletedEvent BookingEventData {booking = booking{status = DRB.COMPLETED}}
  when shouldUpdateRideComplete $ void $ QP.updateHasTakenValidRide booking.riderId
  unless (booking.status == DRB.COMPLETED) $ void $ QRB.updateStatus booking.id DRB.COMPLETED
  whenJust paymentUrl $ QRB.updatePaymentUrl booking.id
  _ <- QRide.updateMultiple updRide.id updRide
  _ <- QFareBreakup.createMany breakups
  void $ QPFS.updateStatus booking.riderId DPFS.PENDING_RATING {rideId = ride.id}
  QPFS.clearCache booking.riderId

  -- uncomment for update api test; booking.paymentMethodId should be present
  -- whenJust booking.paymentMethodId $ \paymentMethodId -> do
  --   merchant <- CQM.findById booking.merchantId >>= fromMaybeM (MerchantNotFound booking.merchantId.getId)
  --   paymentMethod <-
  --     CQMPM.findByIdAndMerchantId paymentMethodId booking.merchantId
  --       >>= fromMaybeM (MerchantPaymentMethodDoesNotExist paymentMethodId.getId)
  --   let dUpdateReq = ACL.PaymentCompletedBuildReq
  --         { bppBookingId,
  --           bppRideId = ride.bppRideId,
  --           paymentMethodInfo = DMPM.mkPaymentMethodInfo paymentMethod,
  --           bppId = booking.providerId,
  --           bppUrl = booking.providerUrl,
  --           transactionId = booking.transactionId,
  --           merchant
  --         }
  --   becknUpdateReq <- ACL.buildUpdateReq dUpdateReq
  --   void . withShortRetry $ CallBPP.update booking.providerUrl becknUpdateReq
  Notify.notifyOnRideCompleted booking updRide
  where
    buildFareBreakup :: MonadFlow m => Id DRB.Booking -> DFareBreakup -> m DFareBreakup.FareBreakup
    buildFareBreakup bookingId DFareBreakup {..} = do
      guid <- generateGUID
      pure
        DFareBreakup.FareBreakup
          { id = guid,
            ..
          }

driverArrivedReqHandler ::
  ( HasFlowEnv m r '["nwAddress" ::: BaseUrl, "smsCfg" ::: SmsConfig],
    CacheFlow m r,
    EsqDBFlow m r,
    MonadFlow m,
    EncFlow m r,
    EsqDBReplicaFlow m r,
    HasHttpClientOptions r c,
    HasLongDurationRetryCfg r c,
    -- HasShortDurationRetryCfg r c, -- uncomment for test update api
    HasField "minTripDistanceForReferralCfg" r (Maybe HighPrecMeters),
    HasFlowEnv m r '["internalEndPointHashMap" ::: HM.HashMap BaseUrl BaseUrl],
    HasBAPMetrics m r,
    EventStreamFlow m r,
    HasField "hotSpotExpiry" r Seconds
  ) =>
  DriverArrivedReq ->
  m ()
driverArrivedReqHandler DriverArrivedReq {..} = do
  let BookingDetails {..} = bookingDetails
  booking <- runInReplica $ QRB.findByBPPBookingId bookingDetails.bppBookingId >>= fromMaybeM (BookingDoesNotExist $ "BppBookingId: " <> bookingDetails.bppBookingId.getId)
  ride <- QRide.findByBPPRideId bppRideId >>= fromMaybeM (RideDoesNotExist $ "BppRideId" <> bppRideId.getId)
  unless (isValidRideStatus ride.status) $ throwError $ RideInvalidStatus "The ride has already started."

  now <- getCurrentTime
  unless (isJust ride.driverArrivalTime) $ do
    _ <- QRide.updateDriverArrival ride.id
    void $ QPFS.updateStatus booking.riderId DPFS.DRIVER_ARRIVED {rideId = ride.id, bookingId = booking.id, trackingUrl = Nothing, driverLocation = Nothing, driverArrivalTime = Just now}
  where
    isValidRideStatus status = status == DRide.NEW

bookingCancelledReqHandler ::
  ( HasFlowEnv m r '["nwAddress" ::: BaseUrl, "smsCfg" ::: SmsConfig],
    CacheFlow m r,
    EsqDBFlow m r,
    MonadFlow m,
    EncFlow m r,
    EsqDBReplicaFlow m r,
    HasHttpClientOptions r c,
    HasLongDurationRetryCfg r c,
    -- HasShortDurationRetryCfg r c, -- uncomment for test update api
    HasField "minTripDistanceForReferralCfg" r (Maybe HighPrecMeters),
    HasFlowEnv m r '["internalEndPointHashMap" ::: HM.HashMap BaseUrl BaseUrl],
    -- HasFlowEnv m r '["isBecknSpecVersion2" ::: Bool],
    HasBAPMetrics m r,
    EventStreamFlow m r,
    HasField "hotSpotExpiry" r Seconds
  ) =>
  BookingCancelledReq ->
  m ()
bookingCancelledReqHandler BookingCancelledReq {..} = do
  booking <- runInReplica $ QRB.findByBPPBookingId bppBookingId >>= fromMaybeM (BookingDoesNotExist $ "BppBookingId: " <> bppBookingId.getId)
  mbRide <- QRide.findActiveByRBId booking.id
  let isRideCancellable = maybe False (\ride -> ride.status `notElem` [DRide.INPROGRESS, DRide.CANCELLED]) mbRide
      bookingAlreadyCancelled = booking.status == DRB.CANCELLED
  unless (isBookingCancellable booking || (isRideCancellable && bookingAlreadyCancelled)) $
    throwError (BookingInvalidStatus (show booking.status))

  logTagInfo ("BookingId-" <> getId booking.id) ("Cancellation reason " <> show cancellationSource)
  let bookingCancellationReason = mkBookingCancellationReason booking.id (mbRide <&> (.id)) cancellationSource booking.merchantId
  merchantConfigs <- CMC.findAllByMerchantOperatingCityId booking.merchantOperatingCityId
  case cancellationSource of
    DBCR.ByUser -> SMC.updateCustomerFraudCounters booking.riderId merchantConfigs
    DBCR.ByDriver -> SMC.updateCancelledByDriverFraudCounters booking.riderId merchantConfigs
    _ -> pure ()
  fork "incrementing fraud counters" $ do
    let merchantOperatingCityId = booking.merchantOperatingCityId
    mFraudDetected <- SMC.anyFraudDetected booking.riderId merchantOperatingCityId merchantConfigs
    whenJust mFraudDetected $ \mc -> SMC.blockCustomer booking.riderId (Just mc.id)
  case mbRide of
    Just ride -> do
      triggerRideCancelledEvent RideEventData {ride = ride{status = DRide.CANCELLED}, personId = booking.riderId, merchantId = booking.merchantId}
    Nothing -> do
      logDebug "No ride found for the booking."
  triggerBookingCancelledEvent BookingEventData {booking = booking{status = DRB.CANCELLED}}
  _ <- QPFS.updateStatus booking.riderId DPFS.IDLE
  unless (booking.status == DRB.CANCELLED) $ void $ QRB.updateStatus booking.id DRB.CANCELLED
  whenJust mbRide $ \ride -> void $ do
    unless (ride.status == DRide.CANCELLED) $ void $ QRide.updateStatus ride.id DRide.CANCELLED
  unless (cancellationSource == DBCR.ByUser) $
    QBCR.upsert bookingCancellationReason
  QPFS.clearCache booking.riderId
  -- notify customer
  Notify.notifyOnBookingCancelled booking cancellationSource
  where
    isBookingCancellable booking =
      booking.status `elem` [DRB.NEW, DRB.CONFIRMED, DRB.AWAITING_REASSIGNMENT, DRB.TRIP_ASSIGNED]

mkBookingCancellationReason ::
  Id DRB.Booking ->
  Maybe (Id DRide.Ride) ->
  DBCR.CancellationSource ->
  Id DMerchant.Merchant ->
  DBCR.BookingCancellationReason
mkBookingCancellationReason bookingId mbRideId cancellationSource merchantId =
  -- cancellationSource merchantId =
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

-- data ValidatedOnUpdateReq
--   = ValidatedRideAssignedReq
--       { bppBookingId :: Id DRB.BPPBooking,
--         bppRideId :: Id DRide.BPPRide,
--         driverName :: Text,
--         driverImage :: Maybe Text,
--         driverMobileNumber :: Text,
--         driverMobileCountryCode :: Maybe Text,
--         driverRating :: Maybe Centesimal,
--         driverRegisteredAt :: UTCTime,
--         isDriverBirthDay :: Bool,
--         isFreeRide :: Bool,
--         otp :: Text,
--         vehicleNumber :: Text,
--         vehicleColor :: Text,
--         vehicleModel :: Text,
--         booking :: DRB.Booking
--       }
--   | ValidatedRideStartedReq
--       { bppBookingId :: Id DRB.BPPBooking,
--         bppRideId :: Id DRide.BPPRide,
--         booking :: DRB.Booking,
--         ride :: DRide.Ride,
--         tripStartLocation :: Maybe LatLong,
--         endOtp_ :: Maybe Text,
--         startOdometerReading :: Maybe Centesimal
--       }
--   | ValidatedRideCompletedReq
--       { bppBookingId :: Id DRB.BPPBooking,
--         bppRideId :: Id DRide.BPPRide,
--         fare :: Money,
--         totalFare :: Money,
--         fareBreakups :: [DFareBreakup],
--         chargeableDistance :: HighPrecMeters,
--         booking :: DRB.Booking,
--         ride :: DRide.Ride,
--         person :: DP.Person,
--         paymentUrl :: Maybe Text,
--         tripEndLocation :: Maybe LatLong,
--         endOdometerReading :: Maybe Centesimal
--       }
--   | ValidatedBookingCancelledReq
--       { bppBookingId :: Id DRB.BPPBooking,
--         cancellationSource :: DBCR.CancellationSource,
--         booking :: DRB.Booking,
--         mbRide :: Maybe DRide.Ride
--       }
--   | ValidatedBookingReallocationReq
--       { bppBookingId :: Id DRB.BPPBooking,
--         bppRideId :: Id DRide.BPPRide,
--         reallocationSource :: DBCR.CancellationSource,
--         booking :: DRB.Booking,
--         ride :: DRide.Ride
--       }
--   | ValidatedDriverArrivedReq
--       { bppBookingId :: Id DRB.BPPBooking,
--         bppRideId :: Id DRide.BPPRide,
--         arrivalTime :: Maybe UTCTime,
--         booking :: DRB.Booking,
--         ride :: DRide.Ride
--       }
--   | ValidatedEstimateRepetitionReq
--       { searchRequestId :: Id DSR.SearchRequest,
--         bppEstimateId :: Id DEstimate.BPPEstimate,
--         bppBookingId :: Id DRB.BPPBooking,
--         bppRideId :: Id DRide.BPPRide,
--         cancellationSource :: DBCR.CancellationSource,
--         booking :: DRB.Booking,
--         ride :: DRide.Ride,
--         searchReq :: DSR.SearchRequest,
--         estimate :: DEstimate.Estimate
--       }
--   | ValidatedNewMessageReq
--       { bppBookingId :: Id DRB.BPPBooking,
--         bppRideId :: Id DRide.BPPRide,
--         message :: Text,
--         booking :: DRB.Booking,
--         ride :: DRide.Ride
--       }
--   | ValidatedSafetyAlertReq
--       { bppBookingId :: Id DRB.BPPBooking,
--         bppRideId :: Id DRide.BPPRide,
--         booking :: DRB.Booking,
--         ride :: DRide.Ride,
--         code :: Text,
--         reason :: Text
--       }
--   | ValidatedStopArrivedReq
--       { booking :: DRB.Booking,
--         ride :: DRide.Ride
--       }

-- data EstimateRepetitionEstimateInfo = EstimateRepetitionEstimateInfo
--   { vehicleVariant :: VehicleVariant,
--     estimatedFare :: Money,
--     discount :: Maybe Money,
--     estimatedTotalFare :: Money,
--     totalFareRange :: DEstimate.FareRange,
--     descriptions :: [Text],
--     estimateBreakupList :: [EstimateBreakupInfo],
--     nightShiftInfo :: Maybe NightShiftInfo,
--     waitingCharges :: WaitingChargesInfo,
--     driversLocation :: [LatLong]
--   }

-- data NightShiftInfo = NightShiftInfo
--   { nightShiftCharge :: Money,
--     nightShiftStart :: TimeOfDay,
--     nightShiftEnd :: TimeOfDay
--   }

-- data WaitingChargesInfo = WaitingChargesInfo
--   { waitingTimeEstimatedThreshold :: Maybe Seconds,
--     waitingChargePerMin :: Maybe Money
--   }

-- data EstimateBreakupInfo = EstimateBreakupInfo
--   { title :: Text,
--     price :: BreakupPriceInfo
--   }

-- data BreakupPriceInfo = BreakupPriceInfo
--   { currency :: Text,
--     value :: Money
--   }

--     RideAssignedDetails {newRideInfo} -> do
--       rideEntity <- buildRideEntity booking updateNewRide newRideInfo
--       rideBookingTransaction bookingNewStatus rideNewStatus booking rideEntity
--       where
--         bookingNewStatus = DB.TRIP_ASSIGNED
--         rideNewStatus = DRide.NEW
--         updateNewRide newRide = newRide{status = rideNewStatus}
--     RideStartedDetails {newRideInfo, rideStartedInfo} -> do
--       rideEntity <- buildRideEntity booking updateRideStarted newRideInfo
--       rideBookingTransaction bookingNewStatus rideNewStatus booking rideEntity
--       where
--         bookingNewStatus = DB.TRIP_ASSIGNED
--         rideNewStatus = DRide.INPROGRESS
--         updateRideStarted newRide =
--           newRide{status = rideNewStatus,
--                   rideStartTime = Just rideStartedInfo.rideStartTime,
--                   driverArrivalTime = rideStartedInfo.driverArrivalTime
--                  }
--     RideCompletedDetails {newRideInfo, rideStartedInfo, rideCompletedInfo} -> do
--       rideEntity <- buildRideEntity booking updateRideCompleted newRideInfo
--       rideBookingTransaction bookingNewStatus rideNewStatus booking rideEntity
--       when (isStatusChanged booking.status bookingNewStatus rideEntity) $ do
--         breakups <- traverse (buildFareBreakup booking.id) rideCompletedInfo.fareBreakups
--         QFareBreakup.deleteAllByBookingId booking.id
--         QFareBreakup.createMany breakups
--         whenJust rideCompletedInfo.paymentUrl $ QB.updatePaymentUrl booking.id
--       where
--         bookingNewStatus = DB.COMPLETED
--         rideNewStatus = DRide.COMPLETED
--         updateRideCompleted newRide =
--           newRide{status = rideNewStatus,
--                   rideStartTime = Just rideStartedInfo.rideStartTime,
--                   driverArrivalTime = rideStartedInfo.driverArrivalTime,
--                   fare = Just rideCompletedInfo.fare,
--                   totalFare = Just rideCompletedInfo.totalFare,
--                   chargeableDistance = Just rideCompletedInfo.chargeableDistance,
--                   -- traveledDistance = Just rideCompletedInfo.traveledDistance, -- did not changed in on_update
--                   rideEndTime = Just rideCompletedInfo.rideEndTime
--                  }

-- buildRideEntity :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => DB.Booking -> (DRide.Ride -> DRide.Ride) -> NewRideInfo -> m RideEntity
-- buildRideEntity booking updRide newRideInfo = do
--   mbExistingRide <- B.runInReplica $ QRide.findByBPPRideId newRideInfo.bppRideId
--   case mbExistingRide of
--     Nothing -> do
--       mbMerchant <- CQM.findById booking.merchantId
--       newRide <- buildNewRide mbMerchant booking newRideInfo
--       pure $ RenewedRide (updRide newRide)
--     Just existingRide -> do
--       unless (existingRide.bookingId == booking.id) $ throwError (InvalidRequest "Invalid rideId")
--       pure $ UpdatedRide {ride = updRide existingRide, rideOldStatus = existingRide.status}

-- onUpdate ValidatedRideAssignedReq {..} = do
--   mbMerchant <- CQM.findById booking.merchantId
--   ride <- buildRide mbMerchant
--   triggerRideCreatedEvent RideEventData {ride = ride, personId = booking.riderId, merchantId = booking.merchantId}
--   let category = case booking.specialLocationTag of
--         Just _ -> "specialLocation"
--         Nothing -> "normal"
--   incrementRideCreatedRequestCount booking.merchantId.getId booking.merchantOperatingCityId.getId category
--   _ <- QRB.updateStatus booking.id DRB.TRIP_ASSIGNED
--   _ <- QRide.createRide ride

--   _ <- QPFS.updateStatus booking.riderId DPFS.RIDE_PICKUP {rideId = ride.id, bookingId = booking.id, trackingUrl = Nothing, otp, vehicleNumber, fromLocation = Maps.getCoordinates booking.fromLocation, driverLocation = Nothing}
--   QPFS.clearCache booking.riderId
--   Notify.notifyOnRideAssigned booking ride
--   when isDriverBirthDay $ do
--     Notify.notifyDriverBirthDay booking.riderId driverName
--   withLongRetry $ CallBPP.callTrack booking ride
--   where
--     buildRide :: MonadFlow m => Maybe DMerchant.Merchant -> m DRide.Ride
--     buildRide mbMerchant = do
--       guid <- generateGUID
--       shortId <- generateShortId
--       now <- getCurrentTime
--       let fromLocation = booking.fromLocation
--           toLocation = case booking.bookingDetails of
--             DRB.OneWayDetails details -> Just details.toLocation
--             DRB.RentalDetails _ -> Nothing
--             DRB.DriverOfferDetails details -> Just details.toLocation
--             DRB.OneWaySpecialZoneDetails details -> Just details.toLocation
--             DRB.InterCityDetails details -> Just details.toLocation
--       let allowedEditLocationAttempts = Just $ maybe 0 (.numOfAllowedEditPickupLocationAttemptsThreshold) mbMerchant
--       return
--         DRide.Ride
--           { id = guid,
--             bookingId = booking.id,
--             merchantId = Just booking.merchantId,
--             merchantOperatingCityId = Just booking.merchantOperatingCityId,
--             status = DRide.NEW,
--             trackingUrl = Nothing,
--             fare = Nothing,
--             totalFare = Nothing,
--             chargeableDistance = Nothing,
--             traveledDistance = Nothing,
--             driverArrivalTime = Nothing,
--             vehicleVariant = booking.vehicleVariant,
--             createdAt = now,
--             updatedAt = now,
--             rideStartTime = Nothing,
--             rideEndTime = Nothing,
--             rideRating = Nothing,
--             safetyCheckStatus = Nothing,
--             isFreeRide = Just isFreeRide,
--             endOtp = Nothing,
--             startOdometerReading = Nothing,
--             endOdometerReading = Nothing,
--             ..
--           }
-- onUpdate ValidatedRideStartedReq {..} = do
--   fork "ride start geohash frequencyUpdater" $ do
--     case tripStartLocation of
--       Just location -> frequencyUpdator booking.merchantId location Nothing TripStart
--       Nothing -> return ()
--   rideStartTime <- getCurrentTime
--   let updRideForStartReq =
--         ride{status = DRide.INPROGRESS,
--              rideStartTime = Just rideStartTime,
--              rideEndTime = Nothing,
--              endOtp = endOtp_,
--              startOdometerReading = startOdometerReading
--             }
--   triggerRideStartedEvent RideEventData {ride = updRideForStartReq, personId = booking.riderId, merchantId = booking.merchantId}
--   _ <- QRide.updateMultiple updRideForStartReq.id updRideForStartReq
--   _ <- QPFS.updateStatus booking.riderId DPFS.RIDE_STARTED {rideId = ride.id, bookingId = booking.id, trackingUrl = ride.trackingUrl, driverLocation = Nothing}
--   QPFS.clearCache booking.riderId
--   fork "notify emergency contacts" $ Notify.notifyRideStartToEmergencyContacts booking ride
--   Notify.notifyOnRideStarted booking ride
-- onUpdate ValidatedRideCompletedReq {..} = do
--   fork "ride end geohash frequencyUpdater" $ do
--     case tripEndLocation of
--       Just location -> frequencyUpdator booking.merchantId location Nothing TripEnd
--       Nothing -> return ()
--   SMC.updateTotalRidesCounters booking.riderId
--   merchantConfigs <- CMC.findAllByMerchantOperatingCityId booking.merchantOperatingCityId
--   SMC.updateTotalRidesInWindowCounters booking.riderId merchantConfigs

--   rideEndTime <- getCurrentTime
--   let updRide =
--         ride{status = DRide.COMPLETED,
--              fare = Just fare,
--              totalFare = Just totalFare,
--              chargeableDistance = Just chargeableDistance,
--              rideEndTime = Just rideEndTime,
--              endOdometerReading = endOdometerReading
--             }
--   breakups <- traverse (buildFareBreakup booking.id) fareBreakups
--   minTripDistanceForReferralCfg <- asks (.minTripDistanceForReferralCfg)
--   let shouldUpdateRideComplete =
--         case minTripDistanceForReferralCfg of
--           Just distance -> updRide.chargeableDistance >= Just distance && not person.hasTakenValidRide
--           Nothing -> True
--   triggerRideEndEvent RideEventData {ride = updRide, personId = booking.riderId, merchantId = booking.merchantId}
--   triggerBookingCompletedEvent BookingEventData {booking = booking{status = DRB.COMPLETED}}
--   when shouldUpdateRideComplete $ void $ QP.updateHasTakenValidRide booking.riderId
--   unless (booking.status == DRB.COMPLETED) $ void $ QRB.updateStatus booking.id DRB.COMPLETED
--   whenJust paymentUrl $ QRB.updatePaymentUrl booking.id
--   _ <- QRide.updateMultiple updRide.id updRide
--   _ <- QFareBreakup.createMany breakups
--   void $ QPFS.updateStatus booking.riderId DPFS.PENDING_RATING {rideId = ride.id}
--   QPFS.clearCache booking.riderId
--   -- uncomment for update api test; booking.paymentMethodId should be present
--   -- whenJust booking.paymentMethodId $ \paymentMethodId -> do
--   --   merchant <- CQM.findById booking.merchantId >>= fromMaybeM (MerchantNotFound booking.merchantId.getId)
--   --   paymentMethod <-
--   --     CQMPM.findByIdAndMerchantId paymentMethodId booking.merchantId
--   --       >>= fromMaybeM (MerchantPaymentMethodDoesNotExist paymentMethodId.getId)
--   --   let dUpdateReq = ACL.PaymentCompletedBuildReq
--   --         { bppBookingId,
--   --           bppRideId = ride.bppRideId,
--   --           paymentMethodInfo = DMPM.mkPaymentMethodInfo paymentMethod,
--   --           bppId = booking.providerId,
--   --           bppUrl = booking.providerUrl,
--   --           transactionId = booking.transactionId,
--   --           merchant
--   --         }
--   --   becknUpdateReq <- ACL.buildUpdateReq dUpdateReq
--   --   void . withShortRetry $ CallBPP.update booking.providerUrl becknUpdateReq

--   Notify.notifyOnRideCompleted booking updRide
--   where
--     buildFareBreakup :: MonadFlow m => Id DRB.Booking -> OnUpdateFareBreakup -> m DFareBreakup.FareBreakup
--     buildFareBreakup bookingId OnUpdateFareBreakup {..} = do
--       guid <- generateGUID
--       pure
--         DFareBreakup.FareBreakup
--           { id = guid,
--             ..
--           }
