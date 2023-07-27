{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module Domain.Action.Beckn.OnUpdate
  ( onUpdate,
    validateRequest,
    OnUpdateReq (..),
    OnUpdateFareBreakup (..),
    EstimateRepetitionEstimateInfo (..),
    NightShiftInfo (..),
    WaitingChargesInfo (..),
    DEstimate.FareRange (..),
    EstimateBreakupInfo (..),
    BreakupPriceInfo (..),
  )
where

import Beckn.Types.Core.Taxi.OnUpdate.OnUpdateEvent.RideCompletedEvent (LocationInfo (..))
import qualified Domain.Types.Booking as SRB
import qualified Domain.Types.BookingCancellationReason as SBCR
import qualified Domain.Types.Estimate as DEstimate
import qualified Domain.Types.FarePolicy.FareBreakup as DFareBreakup
import Domain.Types.Location
import Domain.Types.LocationAddress
import qualified Domain.Types.LocationMapping as DLocationMapping
import qualified Domain.Types.Merchant as DMerchant
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Person.PersonFlowStatus as DPFS
import Domain.Types.Ride
import qualified Domain.Types.Ride as SRide
import qualified Domain.Types.SearchRequest as DSR
import Domain.Types.VehicleVariant
import qualified Kernel.External.Maps as Maps
import Kernel.Prelude
import qualified Kernel.Storage.Esqueleto as DB
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import Kernel.Storage.Esqueleto.Transactionable (runInReplica)
import Kernel.Storage.Hedis.Config (HedisFlow)
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.SessionizerMetrics.Types.Event
import qualified SharedLogic.CallBPP as CallBPP
import qualified SharedLogic.MerchantConfig as SMC
import Storage.CachedQueries.CacheConfig
import qualified Storage.CachedQueries.MerchantConfig as CMC
import qualified Storage.CachedQueries.Person.PersonFlowStatus as QPFS
import qualified Storage.Queries.Booking as QRB
import qualified Storage.Queries.BookingCancellationReason as QBCR
import qualified Storage.Queries.Estimate as QEstimate
import qualified Storage.Queries.FareBreakup as QFareBreakup
import qualified Storage.Queries.Location as QLocation
import qualified Storage.Queries.LocationMapping as QLocationMapping
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.Ride as QRide
import qualified Storage.Queries.SearchRequest as QSR
import Tools.Error
import Tools.Event
import Tools.Maps (LatLong)
import Tools.Metrics (CoreMetrics)
import qualified Tools.Notifications as Notify

-- import qualified Beckn.ACL.Update as ACL -- uncomment for update api test
-- import qualified Storage.CachedQueries.Merchant.MerchantPaymentMethod as CQMPM -- uncomment for update api test
-- import qualified Domain.Types.Merchant.MerchantPaymentMethod as DMPM -- uncomment for update api test
-- import qualified Storage.CachedQueries.Merchant as CQM -- uncomment for update api test

data OnUpdateReq
  = RideAssignedReq
      { bppBookingId :: Id SRB.BPPBooking,
        bppRideId :: Id SRide.BPPRide,
        driverName :: Text,
        driverMobileNumber :: Text,
        driverMobileCountryCode :: Maybe Text,
        driverRating :: Maybe Centesimal,
        driverRegisteredAt :: UTCTime,
        otp :: Text,
        vehicleNumber :: Text,
        vehicleColor :: Text,
        vehicleModel :: Text
      }
  | RideStartedReq
      { bppBookingId :: Id SRB.BPPBooking,
        bppRideId :: Id SRide.BPPRide,
        startLocation :: Maybe LocationInfo
      }
  | RideCompletedReq
      { bppBookingId :: Id SRB.BPPBooking,
        bppRideId :: Id SRide.BPPRide,
        fare :: Money,
        totalFare :: Money,
        fareBreakups :: [OnUpdateFareBreakup],
        chargeableDistance :: HighPrecMeters,
        traveledDistance :: HighPrecMeters,
        paymentUrl :: Maybe Text,
        endLocation :: Maybe LocationInfo
      }
  | BookingCancelledReq
      { bppBookingId :: Id SRB.BPPBooking,
        cancellationSource :: SBCR.CancellationSource
      }
  | BookingReallocationReq
      { bppBookingId :: Id SRB.BPPBooking,
        bppRideId :: Id SRide.BPPRide,
        reallocationSource :: SBCR.CancellationSource
      }
  | DriverArrivedReq
      { bppBookingId :: Id SRB.BPPBooking,
        bppRideId :: Id SRide.BPPRide,
        arrivalTime :: Maybe UTCTime
      }
  | EstimateRepetitionReq
      { searchRequestId :: Id DSR.SearchRequest,
        bppEstimateId :: Id DEstimate.BPPEstimate,
        bppBookingId :: Id SRB.BPPBooking,
        bppRideId :: Id SRide.BPPRide,
        cancellationSource :: SBCR.CancellationSource
      }
  | NewMessageReq
      { bppBookingId :: Id SRB.BPPBooking,
        bppRideId :: Id SRide.BPPRide,
        message :: Text
      }

data ValidatedOnUpdateReq
  = ValidatedRideAssignedReq
      { bppBookingId :: Id SRB.BPPBooking,
        bppRideId :: Id SRide.BPPRide,
        driverName :: Text,
        driverMobileNumber :: Text,
        driverMobileCountryCode :: Maybe Text,
        driverRating :: Maybe Centesimal,
        driverRegisteredAt :: UTCTime,
        otp :: Text,
        vehicleNumber :: Text,
        vehicleColor :: Text,
        vehicleModel :: Text,
        booking :: SRB.Booking
      }
  | ValidatedRideStartedReq
      { bppBookingId :: Id SRB.BPPBooking,
        bppRideId :: Id SRide.BPPRide,
        booking :: SRB.Booking,
        ride :: SRide.Ride,
        startLocation :: Maybe LocationInfo
      }
  | ValidatedRideCompletedReq
      { bppBookingId :: Id SRB.BPPBooking,
        bppRideId :: Id SRide.BPPRide,
        fare :: Money,
        totalFare :: Money,
        fareBreakups :: [OnUpdateFareBreakup],
        chargeableDistance :: HighPrecMeters,
        booking :: SRB.Booking,
        ride :: SRide.Ride,
        person :: DP.Person,
        paymentUrl :: Maybe Text,
        endLocation :: Maybe LocationInfo
      }
  | ValidatedBookingCancelledReq
      { bppBookingId :: Id SRB.BPPBooking,
        cancellationSource :: SBCR.CancellationSource,
        booking :: SRB.Booking,
        mbRide :: Maybe SRide.Ride
      }
  | ValidatedBookingReallocationReq
      { bppBookingId :: Id SRB.BPPBooking,
        bppRideId :: Id SRide.BPPRide,
        reallocationSource :: SBCR.CancellationSource,
        booking :: SRB.Booking,
        ride :: SRide.Ride
      }
  | ValidatedDriverArrivedReq
      { bppBookingId :: Id SRB.BPPBooking,
        bppRideId :: Id SRide.BPPRide,
        arrivalTime :: Maybe UTCTime,
        booking :: SRB.Booking,
        ride :: SRide.Ride
      }
  | ValidatedEstimateRepetitionReq
      { searchRequestId :: Id DSR.SearchRequest,
        bppEstimateId :: Id DEstimate.BPPEstimate,
        bppBookingId :: Id SRB.BPPBooking,
        bppRideId :: Id SRide.BPPRide,
        cancellationSource :: SBCR.CancellationSource,
        booking :: SRB.Booking,
        ride :: SRide.Ride,
        searchReq :: DSR.SearchRequest,
        estimate :: DEstimate.Estimate
      }
  | ValidatedNewMessageReq
      { bppBookingId :: Id SRB.BPPBooking,
        bppRideId :: Id SRide.BPPRide,
        message :: Text,
        booking :: SRB.Booking,
        ride :: SRide.Ride
      }

data OnUpdateFareBreakup = OnUpdateFareBreakup
  { amount :: HighPrecMoney,
    description :: Text
  }

data EstimateRepetitionEstimateInfo = EstimateRepetitionEstimateInfo
  { vehicleVariant :: VehicleVariant,
    estimatedFare :: Money,
    discount :: Maybe Money,
    estimatedTotalFare :: Money,
    totalFareRange :: DEstimate.FareRange,
    descriptions :: [Text],
    estimateBreakupList :: [EstimateBreakupInfo],
    nightShiftInfo :: Maybe NightShiftInfo,
    waitingCharges :: WaitingChargesInfo,
    driversLocation :: [LatLong]
  }

data NightShiftInfo = NightShiftInfo
  { nightShiftCharge :: Money,
    nightShiftStart :: TimeOfDay,
    nightShiftEnd :: TimeOfDay
  }

data WaitingChargesInfo = WaitingChargesInfo
  { waitingTimeEstimatedThreshold :: Maybe Seconds,
    waitingChargePerMin :: Maybe Money
  }

data EstimateBreakupInfo = EstimateBreakupInfo
  { title :: Text,
    price :: BreakupPriceInfo
  }

data BreakupPriceInfo = BreakupPriceInfo
  { currency :: Text,
    value :: Money
  }

onUpdate ::
  ( HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    HasCacheConfig r,
    EsqDBFlow m r,
    EncFlow m r,
    EsqDBReplicaFlow m r,
    CoreMetrics m,
    HasHttpClientOptions r c,
    HasLongDurationRetryCfg r c,
    -- HasShortDurationRetryCfg r c, -- uncomment for test update api
    HedisFlow m r,
    HasField "minTripDistanceForReferralCfg" r (Maybe HighPrecMeters),
    EventStreamFlow m r
  ) =>
  ValidatedOnUpdateReq ->
  m ()
onUpdate ValidatedRideAssignedReq {..} = do
  ride <- buildRide
  triggerRideCreatedEvent RideEventData {ride = ride, personId = booking.riderId, merchantId = booking.merchantId}
  DB.runTransaction $ do
    QRB.updateStatus booking.id SRB.TRIP_ASSIGNED
    QRide.create ride
    QPFS.updateStatus booking.riderId DPFS.RIDE_PICKUP {rideId = ride.id, bookingId = booking.id, trackingUrl = Nothing, otp, vehicleNumber, fromLocation = Maps.getCoordinates booking.fromLocation, driverLocation = Nothing}
  QPFS.clearCache booking.riderId
  Notify.notifyOnRideAssigned booking ride
  withLongRetry $ CallBPP.callTrack booking ride
  where
    buildRide :: MonadFlow m => m SRide.Ride
    buildRide = do
      guid <- generateGUID
      shortId <- generateShortId
      now <- getCurrentTime
      toLocation <- case booking.bookingDetails of
        SRB.OneWayDetails details -> return details.toLocation
        SRB.RentalDetails _ -> return []
        SRB.DriverOfferDetails details -> return details.toLocation
        SRB.OneWaySpecialZoneDetails details -> return details.toLocation
      return
        SRide.Ride
          { id = guid,
            fromLocation = booking.fromLocation,
            bookingId = booking.id,
            merchantId = Just booking.merchantId,
            status = SRide.NEW,
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
            ..
          }
onUpdate ValidatedRideStartedReq {..} = do
  rideStartTime <- getCurrentTime
  let updRideForStartReq =
        ride{status = SRide.INPROGRESS,
             rideStartTime = Just rideStartTime,
             rideEndTime = Nothing
            }
  triggerRideStartedEvent RideEventData {ride = updRideForStartReq, personId = booking.riderId, merchantId = booking.merchantId}
  startingLocation <- case startLocation of
    Nothing -> return Nothing
    Just location -> do
      fromLocationId <- generateGUID
      fromLocation <- buildLocation fromLocationId location
      return (Just fromLocation)
  whenJust
    startingLocation
    ( \actualFromLocation -> do
        fromLocationMapping <- QLocationMapping.findByTagIdAndOrder ride.id.getId 0 >>= fromMaybeM (InternalError "location not found")
        DB.runTransaction $ do
          logDebug $ "actual from location is " <> show actualFromLocation
          QLocation.create actualFromLocation
          logDebug "UPDATED from location "
          QLocationMapping.updateLocationInMapping fromLocationMapping actualFromLocation.id
    )
  DB.runTransaction $ do
    QRide.updateMultiple updRideForStartReq.id updRideForStartReq
    QPFS.updateStatus booking.riderId DPFS.RIDE_STARTED {rideId = ride.id, bookingId = booking.id, trackingUrl = ride.trackingUrl, driverLocation = Nothing}
  QPFS.clearCache booking.riderId
  Notify.notifyOnRideStarted booking ride
onUpdate ValidatedRideCompletedReq {..} = do
  SMC.updateTotalRidesCounters booking.riderId
  merchantConfigs <- CMC.findAllByMerchantId person.merchantId
  SMC.updateTotalRidesInWindowCounters booking.riderId merchantConfigs

  rideEndTime <- getCurrentTime
  let updRide =
        ride{status = SRide.COMPLETED,
             fare = Just fare,
             totalFare = Just totalFare,
             chargeableDistance = Just chargeableDistance,
             rideEndTime = Just rideEndTime
            }
  breakups <- traverse (buildFareBreakup booking.id) fareBreakups
  minTripDistanceForReferralCfg <- asks (.minTripDistanceForReferralCfg)
  let shouldUpdateRideComplete =
        case minTripDistanceForReferralCfg of
          Just distance -> updRide.chargeableDistance >= Just distance && not person.hasTakenValidRide
          Nothing -> True
  triggerRideEndEvent RideEventData {ride = updRide, personId = booking.riderId, merchantId = booking.merchantId}
  triggerBookingCompletedEvent BookingEventData {booking = booking{status = SRB.COMPLETED}}
  DB.runTransaction $ do
    when shouldUpdateRideComplete $
      QP.updateHasTakenValidRide booking.riderId
    unless (booking.status == SRB.COMPLETED) $ QRB.updateStatus booking.id SRB.COMPLETED
    whenJust paymentUrl $ QRB.updatePaymentUrl booking.id
    QRide.updateMultiple updRide.id updRide
    QFareBreakup.createMany breakups
    QPFS.updateStatus booking.riderId DPFS.PENDING_RATING {rideId = ride.id}
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
  endingLocation <- case endLocation of
    Nothing -> return Nothing
    Just location -> do
      toLocationId <- generateGUID
      toLocation <- buildLocation toLocationId location
      return (Just toLocation)
  whenJust
    endingLocation
    ( \actualToLocation ->
        if null ride.toLocation
          then do
            let rideWithIndexes = zip ([1 ..] :: [Int]) [actualToLocation]
            toLocationMappers <- mapM (DLocationMapping.locationMappingInstanceMaker ride DLocationMapping.Ride) rideWithIndexes
            for_ toLocationMappers $ \locMap -> do
              DB.runTransaction $ QLocationMapping.create locMap
          else do
            mappers <- QLocationMapping.findByTagId $ ride.id.getId
            let toLocationMappers = filter (\mapper -> mapper.order /= 0) mappers
            logDebug $ "actual to location is " <> show actualToLocation
            DB.runTransaction $ QLocation.create actualToLocation
            logDebug "UPDATED to location"
            for_ toLocationMappers $ \locMap -> do
              DB.runTransaction $ QLocationMapping.updateLocationInMapping locMap actualToLocation.id
    )

  Notify.notifyOnRideCompleted booking updRide
  where
    buildFareBreakup :: MonadFlow m => Id SRB.Booking -> OnUpdateFareBreakup -> m DFareBreakup.FareBreakup
    buildFareBreakup bookingId OnUpdateFareBreakup {..} = do
      guid <- generateGUID
      pure
        DFareBreakup.FareBreakup
          { id = guid,
            ..
          }
onUpdate ValidatedBookingCancelledReq {..} = do
  logTagInfo ("BookingId-" <> getId booking.id) ("Cancellation reason " <> show cancellationSource)
  bookingCancellationReason <- buildBookingCancellationReason booking.id (mbRide <&> (.id)) cancellationSource booking.merchantId
  merchantConfigs <- CMC.findAllByMerchantId booking.merchantId
  case cancellationSource of
    SBCR.ByUser -> SMC.updateCustomerFraudCounters booking.riderId merchantConfigs
    SBCR.ByDriver -> SMC.updateCancelledByDriverFraudCounters booking.riderId merchantConfigs
    _ -> pure ()
  fork "incrementing fraud counters" $ do
    mFraudDetected <- SMC.anyFraudDetected booking.riderId booking.merchantId merchantConfigs
    whenJust mFraudDetected $ \mc -> SMC.blockCustomer booking.riderId (Just mc.id)
  case mbRide of
    Just ride -> do
      triggerRideCancelledEvent RideEventData {ride = ride{status = SRide.CANCELLED}, personId = booking.riderId, merchantId = booking.merchantId}
    Nothing -> do
      logDebug "No ride found for the booking."
  triggerBookingCancelledEvent BookingEventData {booking = booking{status = SRB.CANCELLED}}
  DB.runTransaction $ do
    unless (booking.status == SRB.CANCELLED) $ QRB.updateStatus booking.id SRB.CANCELLED
    whenJust mbRide $ \ride -> do
      unless (ride.status == SRide.CANCELLED) $ QRide.updateStatus ride.id SRide.CANCELLED
    unless (cancellationSource == SBCR.ByUser) $
      QBCR.upsert bookingCancellationReason
    QPFS.updateStatus booking.riderId DPFS.IDLE
  QPFS.clearCache booking.riderId
  -- notify customer
  Notify.notifyOnBookingCancelled booking cancellationSource
onUpdate ValidatedBookingReallocationReq {..} = do
  mbRide <- QRide.findActiveByRBId booking.id
  bookingCancellationReason <- buildBookingCancellationReason booking.id (mbRide <&> (.id)) reallocationSource booking.merchantId
  DB.runTransaction $ do
    QRB.updateStatus booking.id SRB.AWAITING_REASSIGNMENT
    QRide.updateStatus ride.id SRide.CANCELLED
    QBCR.upsert bookingCancellationReason
  -- notify customer
  Notify.notifyOnBookingReallocated booking
onUpdate ValidatedDriverArrivedReq {..} = do
  now <- getCurrentTime
  unless (isJust ride.driverArrivalTime) $
    DB.runTransaction $ do
      QRide.updateDriverArrival ride.id
      QPFS.updateStatus booking.riderId DPFS.DRIVER_ARRIVED {rideId = ride.id, bookingId = booking.id, trackingUrl = Nothing, driverLocation = Nothing, driverArrivalTime = Just now}
onUpdate ValidatedNewMessageReq {..} = do
  Notify.notifyOnNewMessage booking message
onUpdate ValidatedEstimateRepetitionReq {..} = do
  bookingCancellationReason <- buildBookingCancellationReason booking.id (Just ride.id) cancellationSource booking.merchantId
  logTagInfo ("EstimateId-" <> getId estimate.id) "Estimate repetition."

  DB.runTransaction $ do
    QEstimate.updateStatus estimate.id DEstimate.DRIVER_QUOTE_REQUESTED
    QRB.updateStatus booking.id SRB.REALLOCATED
    QRide.updateStatus ride.id SRide.CANCELLED
    QBCR.upsert bookingCancellationReason
    QPFS.updateStatus searchReq.riderId DPFS.WAITING_FOR_DRIVER_OFFERS {estimateId = estimate.id, validTill = searchReq.validTill}
  QPFS.clearCache searchReq.riderId
  -- notify customer
  Notify.notifyOnEstimatedReallocated booking estimate.id

validateRequest ::
  ( HasCacheConfig r,
    EsqDBFlow m r,
    EsqDBReplicaFlow m r,
    CoreMetrics m,
    HasHttpClientOptions r c,
    HasLongDurationRetryCfg r c,
    MonadFlow m,
    MonadReader r m,
    HedisFlow m r,
    HasField "minTripDistanceForReferralCfg" r (Maybe HighPrecMeters)
  ) =>
  OnUpdateReq ->
  m ValidatedOnUpdateReq
validateRequest RideAssignedReq {..} = do
  booking <- runInReplica $ QRB.findByBPPBookingId bppBookingId >>= fromMaybeM (BookingDoesNotExist $ "BppBookingId: " <> bppBookingId.getId)
  unless (isAssignable booking) $ throwError (BookingInvalidStatus $ show booking.status)
  return $ ValidatedRideAssignedReq {..}
  where
    isAssignable booking = booking.status `elem` [SRB.CONFIRMED, SRB.AWAITING_REASSIGNMENT]
validateRequest RideStartedReq {..} = do
  booking <- runInReplica $ QRB.findByBPPBookingId bppBookingId >>= fromMaybeM (BookingDoesNotExist $ "BppBookingId: " <> bppBookingId.getId)
  ride <- QRide.findByBPPRideId bppRideId >>= fromMaybeM (RideDoesNotExist $ "BppRideId" <> bppRideId.getId)
  unless (booking.status == SRB.TRIP_ASSIGNED) $ throwError (BookingInvalidStatus $ show booking.status)
  unless (ride.status == SRide.NEW) $ throwError (RideInvalidStatus $ show ride.status)
  return $ ValidatedRideStartedReq {..}
validateRequest RideCompletedReq {..} = do
  booking <- runInReplica $ QRB.findByBPPBookingId bppBookingId >>= fromMaybeM (BookingDoesNotExist $ "BppBookingId: " <> bppBookingId.getId)
  ride <- QRide.findByBPPRideId bppRideId >>= fromMaybeM (RideDoesNotExist $ "BppRideId" <> bppRideId.getId)
  let bookingCanBeCompleted = booking.status == SRB.TRIP_ASSIGNED
      rideCanBeCompleted = ride.status == SRide.INPROGRESS
      bookingAlreadyCompleted = booking.status == SRB.COMPLETED
      rideAlreadyCompleted = ride.status == SRide.COMPLETED
  unless (bookingCanBeCompleted || (bookingAlreadyCompleted && rideCanBeCompleted)) $
    throwError (BookingInvalidStatus $ show booking.status)
  unless (rideCanBeCompleted || (rideAlreadyCompleted && bookingCanBeCompleted)) $
    throwError (RideInvalidStatus $ show ride.status)
  person <- QP.findById booking.riderId >>= fromMaybeM (PersonNotFound booking.riderId.getId)
  return $ ValidatedRideCompletedReq {..}
validateRequest BookingCancelledReq {..} = do
  booking <- runInReplica $ QRB.findByBPPBookingId bppBookingId >>= fromMaybeM (BookingDoesNotExist $ "BppBookingId: " <> bppBookingId.getId)
  mbRide <- QRide.findActiveByRBId booking.id
  let isRideCancellable = maybe False (\ride -> ride.status `notElem` [SRide.INPROGRESS, SRide.CANCELLED]) mbRide
      bookingAlreadyCancelled = booking.status == SRB.CANCELLED
  unless (isBookingCancellable booking || (isRideCancellable && bookingAlreadyCancelled)) $
    throwError (BookingInvalidStatus (show booking.status))
  return $ ValidatedBookingCancelledReq {..}
  where
    isBookingCancellable booking =
      booking.status `elem` [SRB.NEW, SRB.CONFIRMED, SRB.AWAITING_REASSIGNMENT, SRB.TRIP_ASSIGNED]
validateRequest BookingReallocationReq {..} = do
  booking <- runInReplica $ QRB.findByBPPBookingId bppBookingId >>= fromMaybeM (BookingDoesNotExist $ "BppBookingId: " <> bppBookingId.getId)
  ride <- QRide.findByBPPRideId bppRideId >>= fromMaybeM (RideDoesNotExist $ "BppRideId" <> bppRideId.getId)
  return $ ValidatedBookingReallocationReq {..}
validateRequest DriverArrivedReq {..} = do
  booking <- runInReplica $ QRB.findByBPPBookingId bppBookingId >>= fromMaybeM (BookingDoesNotExist $ "BppBookingId: " <> bppBookingId.getId)
  ride <- QRide.findByBPPRideId bppRideId >>= fromMaybeM (RideDoesNotExist $ "BppRideId" <> bppRideId.getId)
  unless (isValidRideStatus ride.status) $ throwError $ RideInvalidStatus "The ride has already started."
  return $ ValidatedDriverArrivedReq {..}
  where
    isValidRideStatus status = status == SRide.NEW
validateRequest NewMessageReq {..} = do
  booking <- runInReplica $ QRB.findByBPPBookingId bppBookingId >>= fromMaybeM (BookingDoesNotExist $ "BppBookingId: " <> bppBookingId.getId)
  ride <- QRide.findByBPPRideId bppRideId >>= fromMaybeM (RideDoesNotExist $ "BppRideId" <> bppRideId.getId)
  unless (isValidRideStatus ride.status) $ throwError $ RideInvalidStatus "The ride has already started."
  return $ ValidatedNewMessageReq {..}
  where
    isValidRideStatus status = status == SRide.NEW
validateRequest EstimateRepetitionReq {..} = do
  booking <- runInReplica $ QRB.findByBPPBookingId bppBookingId >>= fromMaybeM (BookingDoesNotExist $ "BppBookingId: " <> bppBookingId.getId)
  searchReq <- QSR.findById searchRequestId >>= fromMaybeM (SearchRequestNotFound searchRequestId.getId)
  ride <- QRide.findByBPPRideId bppRideId >>= fromMaybeM (RideDoesNotExist $ "BppRideId" <> bppRideId.getId)
  estimate <- QEstimate.findByBPPEstimateId bppEstimateId >>= fromMaybeM (EstimateDoesNotExist bppEstimateId.getId)
  return $ ValidatedEstimateRepetitionReq {..}

buildBookingCancellationReason ::
  (HasCacheConfig r, EsqDBFlow m r, HedisFlow m r, CoreMetrics m) =>
  Id SRB.Booking ->
  Maybe (Id SRide.Ride) ->
  SBCR.CancellationSource ->
  Id DMerchant.Merchant ->
  m SBCR.BookingCancellationReason
buildBookingCancellationReason bookingId mbRideId cancellationSource merchantId = do
  return
    SBCR.BookingCancellationReason
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

buildLocation :: (MonadFlow m) => Id Location -> LocationInfo -> m Location
buildLocation locationId location = do
  now <- getCurrentTime
  return $
    Location
      { id = locationId,
        lat = location.gps.lat,
        lon = location.gps.lon,
        address =
          LocationAddress
            { street = location.address.street,
              city = location.address.city,
              state = location.address.state,
              country = location.address.country,
              building = location.address.building,
              areaCode = location.address.area_code,
              area = location.address.locality,
              door = location.address.door,
              ward = location.address.ward,
              placeId = Nothing
            },
        createdAt = now,
        updatedAt = now
      }
