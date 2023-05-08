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
import Domain.Types.VehicleVariant
import Environment
import qualified Kernel.External.Maps as Maps
import Kernel.Prelude
import qualified Kernel.Storage.Esqueleto as DB
import Kernel.Storage.Hedis.Config (HedisFlow)
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified SharedLogic.CallBPP as CallBPP
import Storage.CachedQueries.CacheConfig
import qualified Storage.CachedQueries.Person.PersonFlowStatus as QPFS
import qualified Storage.Queries.Booking as QRB
import qualified Storage.Queries.BookingCancellationReason as QBCR
import qualified Storage.Queries.Estimate as QEstimate
import qualified Storage.Queries.FareBreakup as QFareBreakup
import qualified Storage.Queries.Person as QP
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
        bppEstimateId :: Id DEstimate.BPPEstimate,
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
  OnUpdateReq ->
  m ()
onUpdate RideAssignedReq {..} = do
  booking <- QRB.findByBPPBookingId bppBookingId >>= fromMaybeM (BookingDoesNotExist $ "BppBookingId: " <> bppBookingId.getId)
  unless (isAssignable booking) $ throwError (BookingInvalidStatus $ show booking.status)
  ride <- buildRide booking
  DB.runTransaction $ do
    QRB.updateStatus booking.id SRB.TRIP_ASSIGNED
    QRide.create ride
    QPFS.updateStatus booking.riderId DPFS.RIDE_PICKUP {rideId = ride.id, bookingId = booking.id, trackingUrl = Nothing, otp, vehicleNumber, fromLocation = Maps.getCoordinates booking.fromLocation, driverLocation = Nothing}
  QPFS.clearCache booking.riderId
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
onUpdate RideStartedReq {..} = do
  booking <- QRB.findByBPPBookingId bppBookingId >>= fromMaybeM (BookingDoesNotExist $ "BppBookingId: " <> bppBookingId.getId)
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
    QPFS.updateStatus booking.riderId DPFS.RIDE_STARTED {rideId = ride.id, bookingId = booking.id, trackingUrl = ride.trackingUrl, driverLocation = Nothing}
  QPFS.clearCache booking.riderId
  Notify.notifyOnRideStarted booking ride
onUpdate RideCompletedReq {..} = do
  booking <- QRB.findByBPPBookingId bppBookingId >>= fromMaybeM (BookingDoesNotExist $ "BppBookingId: " <> bppBookingId.getId)
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
  QPFS.clearCache booking.riderId
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
onUpdate BookingCancelledReq {..} = do
  booking <- QRB.findByBPPBookingId bppBookingId >>= fromMaybeM (BookingDoesNotExist $ "BppBookingId: " <> bppBookingId.getId)
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
  QPFS.clearCache booking.riderId
  -- notify customer
  Notify.notifyOnBookingCancelled booking cancellationSource
  where
    isBookingCancellable booking =
      booking.status `elem` [SRB.NEW, SRB.CONFIRMED, SRB.AWAITING_REASSIGNMENT, SRB.TRIP_ASSIGNED]
onUpdate BookingReallocationReq {..} = do
  booking <- QRB.findByBPPBookingId bppBookingId >>= fromMaybeM (BookingDoesNotExist $ "BppBookingId: " <> bppBookingId.getId)
  mbRide <- QRide.findActiveByRBId booking.id
  bookingCancellationReason <- buildBookingCancellationReason booking.id (mbRide <&> (.id)) reallocationSource
  ride <- QRide.findByBPPRideId bppRideId >>= fromMaybeM (RideDoesNotExist $ "BppRideId" <> bppRideId.getId)
  DB.runTransaction $ do
    QRB.updateStatus booking.id SRB.AWAITING_REASSIGNMENT
    QRide.updateStatus ride.id SRide.CANCELLED
    QBCR.upsert bookingCancellationReason
  -- notify customer
  Notify.notifyOnBookingReallocated booking
onUpdate DriverArrivedReq {..} = do
  _booking <- QRB.findByBPPBookingId bppBookingId >>= fromMaybeM (BookingDoesNotExist $ "BppBookingId: " <> bppBookingId.getId)
  ride <- QRide.findByBPPRideId bppRideId >>= fromMaybeM (RideDoesNotExist $ "BppRideId" <> bppRideId.getId)
  unless (isValidRideStatus ride.status) $ throwError $ RideInvalidStatus "The ride has already started."
  unless (isJust ride.driverArrivalTime) $
    DB.runTransaction $ do
      QRide.updateDriverArrival ride.id
  where
    isValidRideStatus status = status == SRide.NEW
onUpdate EstimateRepetitionReq {..} = do
  booking <- QRB.findByBPPBookingId bppBookingId >>= fromMaybeM (BookingDoesNotExist $ "BppBookingId: " <> bppBookingId.getId)
  searchReq <- QSR.findById searchRequestId >>= fromMaybeM (SearchRequestNotFound searchRequestId.getId)
  ride <- QRide.findByBPPRideId bppRideId >>= fromMaybeM (RideDoesNotExist $ "BppRideId" <> bppRideId.getId)
  estimate <- QEstimate.findByBPPEstimateId bppEstimateId >>= fromMaybeM (EstimateDoesNotExist bppEstimateId.getId)
  bookingCancellationReason <- buildBookingCancellationReason booking.id (Just ride.id) cancellationSource
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
