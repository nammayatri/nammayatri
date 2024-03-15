{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wwarn=incomplete-record-updates #-}

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

import qualified Data.HashMap.Strict as HM
import Data.Time hiding (getCurrentTime)
import qualified Domain.Action.Beckn.Common as Common
import qualified Domain.Types.Booking as SRB
import qualified Domain.Types.BookingCancellationReason as SBCR
import qualified Domain.Types.Estimate as DEstimate
import qualified Domain.Types.Merchant as DMerchant
import qualified Domain.Types.Person.PersonFlowStatus as DPFS
import qualified Domain.Types.Ride as SRide
import qualified Domain.Types.SearchRequest as DSR
import Domain.Types.VehicleVariant
import Environment ()
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Sms.Config (SmsConfig)
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.SessionizerMetrics.Types.Event
import qualified Storage.CachedQueries.Person.PersonFlowStatus as QPFS
import qualified Storage.Queries.Booking as QRB
import qualified Storage.Queries.BookingCancellationReason as QBCR
import qualified Storage.Queries.Estimate as QEstimate
import qualified Storage.Queries.Ride as QRide
import qualified Storage.Queries.SearchRequest as QSR
import Tools.Error
import Tools.Maps (LatLong)
import Tools.Metrics (HasBAPMetrics)
import qualified Tools.Notifications as Notify

data OnUpdateReq
  = OURideAssignedReq Common.RideAssignedReq
  | OURideStartedReq Common.RideStartedReq
  | OURideCompletedReq Common.RideCompletedReq
  | OUBookingCancelledReq Common.BookingCancelledReq
  | BookingReallocationReq
      { bppBookingId :: Id SRB.BPPBooking,
        bppRideId :: Id SRide.BPPRide,
        reallocationSource :: SBCR.CancellationSource
      }
  | OUDriverArrivedReq Common.DriverArrivedReq
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
  | SafetyAlertReq
      { bppBookingId :: Id SRB.BPPBooking,
        bppRideId :: Id SRide.BPPRide,
        reason :: Text,
        code :: Text
      }
  | StopArrivedReq
      { bppRideId :: Id SRide.BPPRide
      }

data ValidatedOnUpdateReq
  = ValidatedRideAssignedReq Common.ValidatedRideAssignedReq
  | ValidatedRideStartedReq Common.ValidatedRideStartedReq
  | ValidatedRideCompletedReq Common.ValidatedRideCompletedReq
  | ValidatedBookingCancelledReq Common.ValidatedBookingCancelledReq
  | ValidatedBookingReallocationReq
      { bppBookingId :: Id SRB.BPPBooking,
        bppRideId :: Id SRide.BPPRide,
        reallocationSource :: SBCR.CancellationSource,
        booking :: SRB.Booking,
        ride :: SRide.Ride
      }
  | ValidatedDriverArrivedReq Common.ValidatedDriverArrivedReq
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
  | ValidatedSafetyAlertReq
      { bppBookingId :: Id SRB.BPPBooking,
        bppRideId :: Id SRide.BPPRide,
        booking :: SRB.Booking,
        ride :: SRide.Ride,
        code :: Text,
        reason :: Text
      }
  | ValidatedStopArrivedReq
      { booking :: SRB.Booking,
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
    HasFlowEnv m r '["isBecknSpecVersion2" ::: Bool],
    HasBAPMetrics m r,
    EventStreamFlow m r,
    HasField "hotSpotExpiry" r Seconds
  ) =>
  ValidatedOnUpdateReq ->
  m ()
onUpdate = \case
  ValidatedRideAssignedReq req -> Common.rideAssignedReqHandler req
  ValidatedRideStartedReq req -> Common.rideStartedReqHandler req
  ValidatedRideCompletedReq req -> Common.rideCompletedReqHandler req
  ValidatedBookingCancelledReq req -> Common.bookingCancelledReqHandler req
  ValidatedBookingReallocationReq {..} -> do
    mbRide <- QRide.findActiveByRBId booking.id
    let bookingCancellationReason = mkBookingCancellationReason booking.id (mbRide <&> (.id)) reallocationSource booking.merchantId
    _ <- QRB.updateStatus booking.id SRB.AWAITING_REASSIGNMENT
    _ <- QRide.updateStatus ride.id SRide.CANCELLED
    QBCR.upsert bookingCancellationReason
    Notify.notifyOnBookingReallocated booking
  ValidatedDriverArrivedReq req -> Common.driverArrivedReqHandler req
  ValidatedNewMessageReq {..} -> Notify.notifyOnNewMessage booking message
  ValidatedEstimateRepetitionReq {..} -> do
    let bookingCancellationReason = mkBookingCancellationReason booking.id (Just ride.id) cancellationSource booking.merchantId
    logTagInfo ("EstimateId-" <> getId estimate.id) "Estimate repetition."

    _ <- QEstimate.updateStatus estimate.id DEstimate.DRIVER_QUOTE_REQUESTED
    _ <- QRB.updateStatus booking.id SRB.REALLOCATED
    _ <- QRide.updateStatus ride.id SRide.CANCELLED
    _ <- QBCR.upsert bookingCancellationReason
    _ <- QPFS.updateStatus searchReq.riderId DPFS.WAITING_FOR_DRIVER_OFFERS {estimateId = estimate.id, validTill = searchReq.validTill}
    QPFS.clearCache searchReq.riderId
    -- notify customer
    Notify.notifyOnEstimatedReallocated booking estimate.id
  ValidatedSafetyAlertReq {..} -> Notify.notifySafetyAlert booking code
  ValidatedStopArrivedReq {..} -> do
    QRB.updateStop booking Nothing
    Notify.notifyOnStopReached booking ride

validateRequest ::
  ( CacheFlow m r,
    EsqDBFlow m r,
    EsqDBReplicaFlow m r,
    HasHttpClientOptions r c,
    HasLongDurationRetryCfg r c,
    HasField "minTripDistanceForReferralCfg" r (Maybe HighPrecMeters)
  ) =>
  OnUpdateReq ->
  m ValidatedOnUpdateReq
validateRequest = \case
  OURideAssignedReq req -> do
    validatedRequest <- Common.validateRideAssignedReq req
    return $ ValidatedRideAssignedReq validatedRequest
  OURideStartedReq req -> do
    validatedRequest <- Common.validateRideStartedReq req
    return $ ValidatedRideStartedReq validatedRequest
  OURideCompletedReq req -> do
    validatedRequest <- Common.validateRideCompletedReq req
    return $ ValidatedRideCompletedReq validatedRequest
  OUBookingCancelledReq req -> do
    validatedRequest <- Common.validateBookingCancelledReq req
    return $ ValidatedBookingCancelledReq validatedRequest
  BookingReallocationReq {..} -> do
    booking <- runInReplica $ QRB.findByBPPBookingId bppBookingId >>= fromMaybeM (BookingDoesNotExist $ "BppBookingId:-" <> bppBookingId.getId)
    ride <- QRide.findByBPPRideId bppRideId >>= fromMaybeM (RideDoesNotExist $ "BppRideId" <> bppRideId.getId)
    return $ ValidatedBookingReallocationReq {..}
  OUDriverArrivedReq req -> do
    validatedRequest <- Common.validateDriverArrivedReq req
    return $ ValidatedDriverArrivedReq validatedRequest
  NewMessageReq {..} -> do
    booking <- runInReplica $ QRB.findByBPPBookingId bppBookingId >>= fromMaybeM (BookingDoesNotExist $ "BppBookingId:-" <> bppBookingId.getId)
    ride <- QRide.findByBPPRideId bppRideId >>= fromMaybeM (RideDoesNotExist $ "BppRideId" <> bppRideId.getId)
    unless (isValidRideStatus ride.status) $ throwError $ RideInvalidStatus "The ride has already started."
    return $ ValidatedNewMessageReq {..}
    where
      isValidRideStatus status = status == SRide.NEW
  EstimateRepetitionReq {..} -> do
    booking <- runInReplica $ QRB.findByBPPBookingId bppBookingId >>= fromMaybeM (BookingDoesNotExist $ "BppBookingId:-" <> bppBookingId.getId)
    searchReq <- QSR.findById searchRequestId >>= fromMaybeM (SearchRequestNotFound searchRequestId.getId)
    ride <- QRide.findByBPPRideId bppRideId >>= fromMaybeM (RideDoesNotExist $ "BppRideId" <> bppRideId.getId)
    estimate <- QEstimate.findByBPPEstimateId bppEstimateId >>= fromMaybeM (EstimateDoesNotExist bppEstimateId.getId)
    return $ ValidatedEstimateRepetitionReq {..}
  SafetyAlertReq {..} -> do
    booking <- runInReplica $ QRB.findByBPPBookingId bppBookingId >>= fromMaybeM (BookingDoesNotExist $ "BppBookingId:-" <> bppBookingId.getId)
    unless (booking.status == SRB.TRIP_ASSIGNED) $ throwError (BookingInvalidStatus $ show booking.status)
    ride <- QRide.findByBPPRideId bppRideId >>= fromMaybeM (RideDoesNotExist $ "BppRideId" <> bppRideId.getId)
    unless (ride.status == SRide.INPROGRESS) $ throwError (BookingInvalidStatus "$ show booking.status")
    return ValidatedSafetyAlertReq {..}
  StopArrivedReq {..} -> do
    ride <- QRide.findByBPPRideId bppRideId >>= fromMaybeM (RideDoesNotExist $ "BppRideId" <> bppRideId.getId)
    booking <- runInReplica $ QRB.findById ride.bookingId >>= fromMaybeM (BookingDoesNotExist $ "BppBookingId:-" <> ride.bookingId.getId)
    unless (ride.status == SRide.INPROGRESS) $ throwError $ RideInvalidStatus ("This ride-(" <> ride.id.getId <> ") is not in progress")
    case booking.bookingDetails of
      SRB.OneWayDetails _ -> throwError $ InvalidRequest "Stops are not present in static offer on demand rides"
      SRB.DriverOfferDetails _ -> throwError $ InvalidRequest "Stops are not present in dynamic offer on demand rides"
      SRB.OneWaySpecialZoneDetails _ -> throwError $ InvalidRequest "Stops are not present in on ride otp rides"
      SRB.InterCityDetails _ -> throwError $ InvalidRequest "Stops are not present in intercity rides"
      SRB.RentalDetails SRB.RentalBookingDetails {..} -> do
        unless (isJust stopLocation) $ throwError (InvalidRequest $ "Can't find stop to be reached for bpp ride " <> bppRideId.getId)
        return $ ValidatedStopArrivedReq {..}

mkBookingCancellationReason ::
  Id SRB.Booking ->
  Maybe (Id SRide.Ride) ->
  SBCR.CancellationSource ->
  Id DMerchant.Merchant ->
  SBCR.BookingCancellationReason
mkBookingCancellationReason bookingId mbRideId cancellationSource merchantId =
  -- cancellationSource merchantId =
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
