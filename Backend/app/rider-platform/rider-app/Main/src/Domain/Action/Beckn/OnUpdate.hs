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
    OnUpdateReq (..),
    OnUpdateFareBreakup (..),
    EstimateRepetitionEstimateInfo (..),
    NightShiftInfo (..),
    WaitingChargesInfo (..),
    EstimateBreakupInfo (..),
    BreakupPriceInfo (..),
  )
where

import qualified Domain.Types.Booking as SRB
import qualified Domain.Types.BookingCancellationReason as SBCR
import qualified Domain.Types.Estimate as DEstimate
import qualified Domain.Types.FarePolicy.FareBreakup as DFareBreakup
import qualified Domain.Types.Person.PersonFlowStatus as DPFS
import qualified Domain.Types.Ride as SRide
import qualified Domain.Types.SearchRequest as DSR
import qualified Domain.Types.TripTerms as DTripTerms
import Domain.Types.VehicleVariant
import Environment
import Kernel.Prelude
import qualified Kernel.Storage.Esqueleto as DB
import Kernel.Storage.Hedis.Config (HedisFlow)
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified SharedLogic.CallBPP as CallBPP
import Storage.CachedQueries.CacheConfig
import qualified Storage.CachedQueries.Merchant as QMerch
import qualified Storage.Queries.Booking as QRB
import qualified Storage.Queries.BookingCancellationReason as QBCR
import qualified Storage.Queries.Estimate as QEstimate
import qualified Storage.Queries.FareBreakup as QFareBreakup
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.Person.PersonFlowStatus as QPFS
import qualified Storage.Queries.Ride as QRide
import qualified Storage.Queries.SearchRequest as QSR
import Tools.Error
import Tools.Maps (LatLong)
import Tools.Metrics (CoreMetrics)
import qualified Tools.Notifications as Notify

data OnUpdateReq
  = RideAssignedReq
      { bppBookingId :: Id SRB.BPPBooking,
        bppRideId :: Id SRide.BPPRide,
        driverName :: Text,
        driverMobileNumber :: Text,
        driverRating :: Maybe Centesimal,
        driverRegisteredAt :: UTCTime,
        otp :: Text,
        vehicleNumber :: Text,
        vehicleColor :: Text,
        vehicleModel :: Text
      }
  | RideStartedReq
      { bppBookingId :: Id SRB.BPPBooking,
        bppRideId :: Id SRide.BPPRide
      }
  | RideCompletedReq
      { bppBookingId :: Id SRB.BPPBooking,
        bppRideId :: Id SRide.BPPRide,
        fare :: Money,
        totalFare :: Money,
        fareBreakups :: [OnUpdateFareBreakup],
        chargeableDistance :: HighPrecMeters
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
        estimateInfo :: EstimateRepetitionEstimateInfo,
        bppBookingId :: Id SRB.BPPBooking,
        bppRideId :: Id SRide.BPPRide,
        cancellationSource :: SBCR.CancellationSource
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
    nightShiftRate :: Maybe NightShiftInfo,
    waitingCharges :: Maybe WaitingChargesInfo,
    driversLocation :: [LatLong]
  }

data NightShiftInfo = NightShiftInfo
  { nightShiftMultiplier :: Maybe Centesimal,
    nightShiftStart :: Maybe TimeOfDay,
    nightShiftEnd :: Maybe TimeOfDay
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
  ( HasCacheConfig r,
    EsqDBFlow m r,
    CoreMetrics m,
    HasBapInfo r m,
    HasHttpClientOptions r c,
    HasLongDurationRetryCfg r c,
    HasFlowEnv
      m
      r
      '["bapSelfIds" ::: BAPs Text, "bapSelfURIs" ::: BAPs BaseUrl],
    HedisFlow m r,
    HasField "minTripDistanceForReferralCfg" r (Maybe HighPrecMeters)
  ) =>
  BaseUrl ->
  OnUpdateReq ->
  m ()
onUpdate registryUrl RideAssignedReq {..} = do
  booking <- QRB.findByBPPBookingId bppBookingId >>= fromMaybeM (BookingDoesNotExist $ "BppBookingId: " <> bppBookingId.getId)
  -- TODO: this supposed to be temporary solution. Check if we still need it
  merchant <- QMerch.findById booking.merchantId >>= fromMaybeM (MerchantNotFound booking.merchantId.getId)
  unless (merchant.registryUrl == registryUrl) $ throwError (InvalidRequest "Merchant doesnt't work with passed url.")

  unless (isAssignable booking) $ throwError (BookingInvalidStatus $ show booking.status)
  ride <- buildRide booking
  DB.runTransaction $ do
    QRB.updateStatus booking.id SRB.TRIP_ASSIGNED
    QRide.create ride
    QPFS.updateStatus booking.riderId DPFS.RIDE_ASSIGNED {rideId = ride.id}
  Notify.notifyOnRideAssigned booking ride
  withLongRetry $ CallBPP.callTrack booking ride
  where
    buildRide :: MonadFlow m => SRB.Booking -> m SRide.Ride
    buildRide booking = do
      guid <- generateGUID
      shortId <- generateShortId
      now <- getCurrentTime
      return
        SRide.Ride
          { id = guid,
            bookingId = booking.id,
            status = SRide.NEW,
            trackingUrl = Nothing,
            fare = Nothing,
            totalFare = Nothing,
            chargeableDistance = Nothing,
            driverArrivalTime = Nothing,
            vehicleVariant = booking.vehicleVariant,
            createdAt = now,
            updatedAt = now,
            rideStartTime = Nothing,
            rideEndTime = Nothing,
            rideRating = Nothing,
            ..
          }
    isAssignable booking = booking.status `elem` [SRB.CONFIRMED, SRB.AWAITING_REASSIGNMENT]
onUpdate registryUrl RideStartedReq {..} = do
  booking <- QRB.findByBPPBookingId bppBookingId >>= fromMaybeM (BookingDoesNotExist $ "BppBookingId: " <> bppBookingId.getId)

  -- TODO: this supposed to be temporary solution. Check if we still need it
  merchant <- QMerch.findById booking.merchantId >>= fromMaybeM (MerchantNotFound booking.merchantId.getId)
  unless (merchant.registryUrl == registryUrl) $ throwError (InvalidRequest "Merchant doesnt't work with passed url.")

  ride <- QRide.findByBPPRideId bppRideId >>= fromMaybeM (RideDoesNotExist $ "BppRideId" <> bppRideId.getId)
  unless (booking.status == SRB.TRIP_ASSIGNED) $ throwError (BookingInvalidStatus $ show booking.status)
  unless (ride.status == SRide.NEW) $ throwError (RideInvalidStatus $ show ride.status)
  rideStartTime <- getCurrentTime
  let updRideForStartReq =
        ride{status = SRide.INPROGRESS,
             rideStartTime = Just rideStartTime,
             rideEndTime = Nothing
            }
  DB.runTransaction $ do
    QRide.updateMultiple updRideForStartReq.id updRideForStartReq
  Notify.notifyOnRideStarted booking ride
onUpdate registryUrl RideCompletedReq {..} = do
  booking <- QRB.findByBPPBookingId bppBookingId >>= fromMaybeM (BookingDoesNotExist $ "BppBookingId: " <> bppBookingId.getId)

  -- TODO: this supposed to be temporary solution. Check if we still need it
  merchant <- QMerch.findById booking.merchantId >>= fromMaybeM (MerchantNotFound booking.merchantId.getId)
  unless (merchant.registryUrl == registryUrl) $ throwError (InvalidRequest "Merchant doesnt't work with passed url.")

  ride <- QRide.findByBPPRideId bppRideId >>= fromMaybeM (RideDoesNotExist $ "BppRideId" <> bppRideId.getId)
  unless (booking.status == SRB.TRIP_ASSIGNED) $ throwError (BookingInvalidStatus $ show booking.status)
  unless (ride.status == SRide.INPROGRESS) $ throwError (RideInvalidStatus $ show ride.status)
  rideEndTime <- getCurrentTime
  let updRide =
        ride{status = SRide.COMPLETED,
             fare = Just fare,
             totalFare = Just totalFare,
             chargeableDistance = Just chargeableDistance,
             rideEndTime = Just rideEndTime
            }
  breakups <- traverse (buildFareBreakup booking.id) fareBreakups
  person <- QP.findById booking.riderId >>= fromMaybeM (PersonNotFound booking.riderId.getId)
  minTripDistanceForReferralCfg <- asks (.minTripDistanceForReferralCfg)
  let shouldUpdateRideComplete =
        case minTripDistanceForReferralCfg of
          Just distance -> updRide.chargeableDistance >= Just distance && not person.hasTakenValidRide
          Nothing -> True
  DB.runTransaction $ do
    when shouldUpdateRideComplete $
      QP.updateHasTakenValidRide booking.riderId
    QRB.updateStatus booking.id SRB.COMPLETED
    QRide.updateMultiple updRide.id updRide
    QFareBreakup.createMany breakups
    QPFS.updateStatus booking.riderId DPFS.PENDING_RATING {rideId = ride.id}
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
onUpdate registryUrl BookingCancelledReq {..} = do
  booking <- QRB.findByBPPBookingId bppBookingId >>= fromMaybeM (BookingDoesNotExist $ "BppBookingId: " <> bppBookingId.getId)

  -- TODO: this supposed to be temporary solution. Check if we still need it
  merchant <- QMerch.findById booking.merchantId >>= fromMaybeM (MerchantNotFound booking.merchantId.getId)
  unless (merchant.registryUrl == registryUrl) $ throwError (InvalidRequest "Merchant doesnt't work with passed url.")

  unless (isBookingCancellable booking) $
    throwError (BookingInvalidStatus (show booking.status))
  mbRide <- QRide.findActiveByRBId booking.id
  logTagInfo ("BookingId-" <> getId booking.id) ("Cancellation reason " <> show cancellationSource)
  bookingCancellationReason <- buildBookingCancellationReason booking.id (mbRide <&> (.id)) cancellationSource
  DB.runTransaction $ do
    QRB.updateStatus booking.id SRB.CANCELLED
    whenJust mbRide $ \ride -> QRide.updateStatus ride.id SRide.CANCELLED
    unless (cancellationSource == SBCR.ByUser) $
      QBCR.upsert bookingCancellationReason
    QPFS.updateStatus booking.riderId DPFS.IDLE
  -- notify customer
  Notify.notifyOnBookingCancelled booking cancellationSource
  where
    isBookingCancellable booking =
      booking.status `elem` [SRB.NEW, SRB.CONFIRMED, SRB.AWAITING_REASSIGNMENT, SRB.TRIP_ASSIGNED]
onUpdate registryUrl BookingReallocationReq {..} = do
  booking <- QRB.findByBPPBookingId bppBookingId >>= fromMaybeM (BookingDoesNotExist $ "BppBookingId: " <> bppBookingId.getId)
  mbRide <- QRide.findActiveByRBId booking.id
  bookingCancellationReason <- buildBookingCancellationReason booking.id (mbRide <&> (.id)) reallocationSource
  -- TODO: this supposed to be temporary solution. Check if we still need it
  merchant <- QMerch.findById booking.merchantId >>= fromMaybeM (MerchantNotFound booking.merchantId.getId)
  unless (merchant.registryUrl == registryUrl) $ throwError (InvalidRequest "Merchant doesnt't work with passed url.")

  ride <- QRide.findByBPPRideId bppRideId >>= fromMaybeM (RideDoesNotExist $ "BppRideId" <> bppRideId.getId)
  DB.runTransaction $ do
    QRB.updateStatus booking.id SRB.AWAITING_REASSIGNMENT
    QRide.updateStatus ride.id SRide.CANCELLED
    QBCR.upsert bookingCancellationReason
  -- notify customer
  Notify.notifyOnBookingReallocated booking
onUpdate registryUrl DriverArrivedReq {..} = do
  booking <- QRB.findByBPPBookingId bppBookingId >>= fromMaybeM (BookingDoesNotExist $ "BppBookingId: " <> bppBookingId.getId)
  merchant <- QMerch.findById booking.merchantId >>= fromMaybeM (MerchantNotFound booking.merchantId.getId)
  unless (merchant.registryUrl == registryUrl) $ throwError (InvalidRequest "Merchant doesnt't work with passed url.")
  ride <- QRide.findByBPPRideId bppRideId >>= fromMaybeM (RideDoesNotExist $ "BppRideId" <> bppRideId.getId)
  unless (isValidRideStatus ride.status) $ throwError $ RideInvalidStatus "The ride has already started."
  unless (isJust ride.driverArrivalTime) $
    DB.runTransaction $ do
      QRide.updateDriverArrival ride.id
  where
    isValidRideStatus status = status == SRide.NEW
onUpdate registryUrl EstimateRepetitionReq {..} = do
  booking <- QRB.findByBPPBookingId bppBookingId >>= fromMaybeM (BookingDoesNotExist $ "BppBookingId: " <> bppBookingId.getId)
  -- TODO: this supposed to be temporary solution. Check if we still need it
  merchant <- QMerch.findById booking.merchantId >>= fromMaybeM (MerchantNotFound booking.merchantId.getId)
  unless (merchant.registryUrl == registryUrl) $ throwError (InvalidRequest "Merchant doesnt't work with passed url.")

  searchReq <- QSR.findById searchRequestId >>= fromMaybeM (SearchRequestNotFound searchRequestId.getId)
  ride <- QRide.findByBPPRideId bppRideId >>= fromMaybeM (RideDoesNotExist $ "BppRideId" <> bppRideId.getId)
  bookingCancellationReason <- buildBookingCancellationReason booking.id (Just ride.id) cancellationSource

  newEstimate <- buildEstimate searchReq.id booking estimateInfo

  DB.runTransaction $ do
    QRB.updateStatus booking.id SRB.CANCELLED
    QRide.updateStatus ride.id SRide.CANCELLED
    QBCR.upsert bookingCancellationReason
    QEstimate.create newEstimate
    QPFS.updateStatus searchReq.riderId DPFS.WAITING_FOR_DRIVER_OFFERS {estimateId = newEstimate.id, validTill = searchReq.validTill}
  -- notify customer
  Notify.notifyOnEstimatedReallocated booking newEstimate.id
  where
    buildEstimate ::
      MonadFlow m =>
      Id DSR.SearchRequest ->
      SRB.Booking ->
      EstimateRepetitionEstimateInfo ->
      m DEstimate.Estimate
    buildEstimate requestId booking EstimateRepetitionEstimateInfo {..} = do
      now <- getCurrentTime
      uid <- generateGUID
      tripTerms <- buildTripTerms descriptions
      estimateBreakupList' <- buildEstimateBreakUp estimateBreakupList uid
      pure
        DEstimate.Estimate
          { id = uid,
            providerMobileNumber = booking.providerMobileNumber,
            providerName = booking.providerName,
            providerCompletedRidesCount = 0, -- FIXME
            providerId = booking.providerId,
            providerUrl = booking.providerUrl,
            createdAt = now,
            updatedAt = now,
            status = Just DEstimate.NEW,
            estimateBreakupList = estimateBreakupList',
            driversLocation = driversLocation,
            nightShiftRate =
              Just $
                DEstimate.NightShiftRate
                  { nightShiftMultiplier = nightShiftRate >>= (.nightShiftMultiplier),
                    nightShiftStart = nightShiftRate >>= (.nightShiftStart),
                    nightShiftEnd = nightShiftRate >>= (.nightShiftEnd)
                  },
            waitingCharges =
              DEstimate.WaitingCharges
                { waitingChargePerMin = waitingCharges >>= (.waitingChargePerMin),
                  waitingTimeEstimatedThreshold = waitingCharges >>= (.waitingTimeEstimatedThreshold)
                },
            ..
          }

    buildTripTerms ::
      MonadFlow m =>
      [Text] ->
      m (Maybe DTripTerms.TripTerms)
    buildTripTerms [] = pure Nothing
    buildTripTerms descriptions = do
      id <- generateGUID
      pure . Just $ DTripTerms.TripTerms {..}

    buildEstimateBreakUp ::
      MonadFlow m =>
      [EstimateBreakupInfo] ->
      Id DEstimate.Estimate ->
      m [DEstimate.EstimateBreakup]
    buildEstimateBreakUp estimatesItems estId =
      estimatesItems
        `for` \estimateItem -> do
          id <- generateGUID
          price' <- mkEstimatePrice estimateItem.price
          pure
            DEstimate.EstimateBreakup
              { title = estimateItem.title,
                price = price',
                estimateId = estId,
                ..
              }

    mkEstimatePrice ::
      MonadFlow m =>
      BreakupPriceInfo ->
      m DEstimate.EstimateBreakupPrice
    mkEstimatePrice BreakupPriceInfo {..} = pure DEstimate.EstimateBreakupPrice {..}

buildBookingCancellationReason ::
  (HasCacheConfig r, EsqDBFlow m r, HedisFlow m r, CoreMetrics m) =>
  Id SRB.Booking ->
  Maybe (Id SRide.Ride) ->
  SBCR.CancellationSource ->
  m SBCR.BookingCancellationReason
buildBookingCancellationReason bookingId mbRideId cancellationSource = do
  return
    SBCR.BookingCancellationReason
      { bookingId = bookingId,
        rideId = mbRideId,
        source = cancellationSource,
        reasonCode = Nothing,
        reasonStage = Nothing,
        additionalInfo = Nothing
      }
