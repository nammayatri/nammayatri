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
    EstimateRepetitionReq (..),
    NewMessageReq (..),
    SafetyAlertReq (..),
    StopArrivedReq (..),
  )
where

import qualified Data.HashMap.Strict as HM
import Data.Time hiding (getCurrentTime)
import qualified Domain.Action.Beckn.Common as Common
import qualified Domain.Types.Booking as DRB
import qualified Domain.Types.BookingCancellationReason as DBCR
import qualified Domain.Types.Estimate as DEstimate
import qualified Domain.Types.Merchant as DMerchant
import qualified Domain.Types.Person.PersonFlowStatus as DPFS
import qualified Domain.Types.Ride as DRide
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
  | OUBookingReallocationReq BookingReallocationReq -- not used
  | OUDriverArrivedReq Common.DriverArrivedReq
  | OUEstimateRepetitionReq EstimateRepetitionReq
  | OUNewMessageReq NewMessageReq
  | OUSafetyAlertReq SafetyAlertReq
  | OUStopArrivedReq StopArrivedReq

data ValidatedOnUpdateReq
  = OUValidatedRideAssignedReq Common.ValidatedRideAssignedReq
  | OUValidatedRideStartedReq Common.ValidatedRideStartedReq
  | OUValidatedRideCompletedReq Common.ValidatedRideCompletedReq
  | OUValidatedFarePaidReq Common.ValidatedFarePaidReq
  | OUValidatedBookingCancelledReq Common.ValidatedBookingCancelledReq
  | OUValidatedBookingReallocationReq ValidatedBookingReallocationReq
  | OUValidatedDriverArrivedReq Common.ValidatedDriverArrivedReq
  | OUValidatedEstimateRepetitionReq ValidatedEstimateRepetitionReq
  | OUValidatedNewMessageReq ValidatedNewMessageReq
  | OUValidatedSafetyAlertReq ValidatedSafetyAlertReq
  | OUValidatedStopArrivedReq ValidatedStopArrivedReq

data BookingReallocationReq = BookingReallocationReq
  { bppBookingId :: Id DRB.BPPBooking,
    bppRideId :: Id DRide.BPPRide,
    reallocationSource :: DBCR.CancellationSource
  }

data ValidatedBookingReallocationReq = ValidatedBookingReallocationReq
  { bppBookingId :: Id DRB.BPPBooking,
    bppRideId :: Id DRide.BPPRide,
    reallocationSource :: DBCR.CancellationSource,
    booking :: DRB.Booking,
    ride :: DRide.Ride
  }

data EstimateRepetitionReq = EstimateRepetitionReq
  { searchRequestId :: Id DSR.SearchRequest,
    bppEstimateId :: Id DEstimate.BPPEstimate,
    bppBookingId :: Id DRB.BPPBooking,
    bppRideId :: Id DRide.BPPRide,
    cancellationSource :: DBCR.CancellationSource
  }

data ValidatedEstimateRepetitionReq = ValidatedEstimateRepetitionReq
  { searchRequestId :: Id DSR.SearchRequest,
    bppEstimateId :: Id DEstimate.BPPEstimate,
    bppBookingId :: Id DRB.BPPBooking,
    bppRideId :: Id DRide.BPPRide,
    cancellationSource :: DBCR.CancellationSource,
    booking :: DRB.Booking,
    ride :: DRide.Ride,
    searchReq :: DSR.SearchRequest,
    estimate :: DEstimate.Estimate
  }

data NewMessageReq = NewMessageReq
  { bppBookingId :: Id DRB.BPPBooking,
    bppRideId :: Id DRide.BPPRide,
    message :: Text
  }

data ValidatedNewMessageReq = ValidatedNewMessageReq
  { bppBookingId :: Id DRB.BPPBooking,
    bppRideId :: Id DRide.BPPRide,
    message :: Text,
    booking :: DRB.Booking,
    ride :: DRide.Ride
  }

data SafetyAlertReq = SafetyAlertReq
  { bppBookingId :: Id DRB.BPPBooking,
    bppRideId :: Id DRide.BPPRide,
    reason :: Text,
    code :: Text
  }

data ValidatedSafetyAlertReq = ValidatedSafetyAlertReq
  { bppBookingId :: Id DRB.BPPBooking,
    bppRideId :: Id DRide.BPPRide,
    reason :: Text,
    code :: Text,
    booking :: DRB.Booking,
    ride :: DRide.Ride
  }

newtype StopArrivedReq = StopArrivedReq
  { bppRideId :: Id DRide.BPPRide
  }

data ValidatedStopArrivedReq = ValidatedStopArrivedReq
  { bppRideId :: Id DRide.BPPRide,
    booking :: DRB.Booking,
    ride :: DRide.Ride
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
    KvDbFlow m r,
    EncFlow m r,
    EsqDBReplicaFlow m r,
    HasHttpClientOptions r c,
    HasLongDurationRetryCfg r c,
    -- HasShortDurationRetryCfg r c, -- uncomment for test update api
    HasField "minTripDistanceForReferralCfg" r (Maybe Distance),
    HasFlowEnv m r '["internalEndPointHashMap" ::: HM.HashMap BaseUrl BaseUrl],
    HasBAPMetrics m r,
    EventStreamFlow m r,
    HasField "hotSpotExpiry" r Seconds
  ) =>
  ValidatedOnUpdateReq ->
  m ()
onUpdate = \case
  OUValidatedRideAssignedReq req -> Common.rideAssignedReqHandler req
  OUValidatedRideStartedReq req -> Common.rideStartedReqHandler req
  OUValidatedRideCompletedReq req -> Common.rideCompletedReqHandler req
  OUValidatedFarePaidReq req -> Common.farePaidReqHandler req
  OUValidatedBookingCancelledReq req -> Common.bookingCancelledReqHandler req
  OUValidatedBookingReallocationReq ValidatedBookingReallocationReq {..} -> do
    mbRide <- QRide.findActiveByRBId booking.id
    let bookingCancellationReason = mkBookingCancellationReason booking.id (mbRide <&> (.id)) reallocationSource booking.merchantId
    _ <- QRB.updateStatus booking.id DRB.AWAITING_REASSIGNMENT
    _ <- QRide.updateStatus ride.id DRide.CANCELLED
    QBCR.upsert bookingCancellationReason
    Notify.notifyOnBookingReallocated booking
  OUValidatedDriverArrivedReq req -> Common.driverArrivedReqHandler req
  OUValidatedNewMessageReq ValidatedNewMessageReq {..} -> Notify.notifyOnNewMessage booking message
  OUValidatedEstimateRepetitionReq ValidatedEstimateRepetitionReq {..} -> do
    let bookingCancellationReason = mkBookingCancellationReason booking.id (Just ride.id) cancellationSource booking.merchantId
    logTagInfo ("EstimateId-" <> getId estimate.id) "Estimate repetition."

    _ <- QEstimate.updateStatus estimate.id DEstimate.DRIVER_QUOTE_REQUESTED
    _ <- QRB.updateStatus booking.id DRB.REALLOCATED
    _ <- QRide.updateStatus ride.id DRide.CANCELLED
    _ <- QBCR.upsert bookingCancellationReason
    _ <- QPFS.updateStatus searchReq.riderId DPFS.WAITING_FOR_DRIVER_OFFERS {estimateId = estimate.id, validTill = searchReq.validTill}
    QPFS.clearCache searchReq.riderId
    -- notify customer
    Notify.notifyOnEstimatedReallocated booking estimate.id
  OUValidatedSafetyAlertReq ValidatedSafetyAlertReq {..} -> Notify.notifySafetyAlert booking code
  OUValidatedStopArrivedReq ValidatedStopArrivedReq {..} -> do
    QRB.updateStop booking Nothing
    Notify.notifyOnStopReached booking ride

validateRequest ::
  ( KvDbFlow m r,
    EsqDBReplicaFlow m r,
    HasHttpClientOptions r c,
    HasLongDurationRetryCfg r c,
    HasField "minTripDistanceForReferralCfg" r (Maybe Distance)
  ) =>
  OnUpdateReq ->
  m ValidatedOnUpdateReq
validateRequest = \case
  OURideAssignedReq req -> do
    validatedRequest <- Common.validateRideAssignedReq req
    return $ OUValidatedRideAssignedReq validatedRequest
  OURideStartedReq req -> do
    validatedRequest <- Common.validateRideStartedReq req
    return $ OUValidatedRideStartedReq validatedRequest
  OURideCompletedReq req -> do
    vRequest <- Common.validateRideCompletedReq req
    case vRequest of
      Left validatedRequest -> return $ OUValidatedRideCompletedReq validatedRequest
      Right validatedRequest -> return $ OUValidatedFarePaidReq validatedRequest
  OUBookingCancelledReq req -> do
    validatedRequest <- Common.validateBookingCancelledReq req
    return $ OUValidatedBookingCancelledReq validatedRequest
  OUBookingReallocationReq BookingReallocationReq {..} -> do
    booking <- runInReplica $ QRB.findByBPPBookingId bppBookingId >>= fromMaybeM (BookingDoesNotExist $ "BppBookingId:-" <> bppBookingId.getId)
    ride <- QRide.findByBPPRideId bppRideId >>= fromMaybeM (RideDoesNotExist $ "BppRideId" <> bppRideId.getId)
    return $ OUValidatedBookingReallocationReq ValidatedBookingReallocationReq {..}
  OUDriverArrivedReq req -> do
    validatedRequest <- Common.validateDriverArrivedReq req
    return $ OUValidatedDriverArrivedReq validatedRequest
  OUNewMessageReq NewMessageReq {..} -> do
    booking <- runInReplica $ QRB.findByBPPBookingId bppBookingId >>= fromMaybeM (BookingDoesNotExist $ "BppBookingId:-" <> bppBookingId.getId)
    ride <- QRide.findByBPPRideId bppRideId >>= fromMaybeM (RideDoesNotExist $ "BppRideId" <> bppRideId.getId)
    unless (isValidRideStatus ride.status) $ throwError $ RideInvalidStatus "The ride has already started."
    return $ OUValidatedNewMessageReq ValidatedNewMessageReq {..}
    where
      isValidRideStatus status = status `elem` [DRide.NEW, DRide.INPROGRESS]
  OUEstimateRepetitionReq EstimateRepetitionReq {..} -> do
    booking <- runInReplica $ QRB.findByBPPBookingId bppBookingId >>= fromMaybeM (BookingDoesNotExist $ "BppBookingId:-" <> bppBookingId.getId)
    searchReq <- QSR.findById searchRequestId >>= fromMaybeM (SearchRequestNotFound searchRequestId.getId)
    ride <- QRide.findByBPPRideId bppRideId >>= fromMaybeM (RideDoesNotExist $ "BppRideId" <> bppRideId.getId)
    estimate <- QEstimate.findByBPPEstimateId bppEstimateId >>= fromMaybeM (EstimateDoesNotExist bppEstimateId.getId)
    return $ OUValidatedEstimateRepetitionReq ValidatedEstimateRepetitionReq {..}
  OUSafetyAlertReq SafetyAlertReq {..} -> do
    booking <- runInReplica $ QRB.findByBPPBookingId bppBookingId >>= fromMaybeM (BookingDoesNotExist $ "BppBookingId:-" <> bppBookingId.getId)
    unless (booking.status == DRB.TRIP_ASSIGNED) $ throwError (BookingInvalidStatus $ show booking.status)
    ride <- QRide.findByBPPRideId bppRideId >>= fromMaybeM (RideDoesNotExist $ "BppRideId" <> bppRideId.getId)
    unless (ride.status == DRide.INPROGRESS) $ throwError (BookingInvalidStatus "$ show booking.status")
    return $ OUValidatedSafetyAlertReq ValidatedSafetyAlertReq {..}
  OUStopArrivedReq StopArrivedReq {..} -> do
    ride <- QRide.findByBPPRideId bppRideId >>= fromMaybeM (RideDoesNotExist $ "BppRideId" <> bppRideId.getId)
    booking <- runInReplica $ QRB.findById ride.bookingId >>= fromMaybeM (BookingDoesNotExist $ "BppBookingId:-" <> ride.bookingId.getId)
    unless (ride.status == DRide.INPROGRESS) $ throwError $ RideInvalidStatus ("This ride-(" <> ride.id.getId <> ") is not in progress")
    case booking.bookingDetails of
      DRB.OneWayDetails _ -> throwError $ InvalidRequest "Stops are not present in static offer on demand rides"
      DRB.DriverOfferDetails _ -> throwError $ InvalidRequest "Stops are not present in dynamic offer on demand rides"
      DRB.OneWaySpecialZoneDetails _ -> throwError $ InvalidRequest "Stops are not present in on ride otp rides"
      DRB.InterCityDetails _ -> throwError $ InvalidRequest "Stops are not present in intercity rides"
      DRB.RentalDetails DRB.RentalBookingDetails {..} -> do
        unless (isJust stopLocation) $ throwError (InvalidRequest $ "Can't find stop to be reached for bpp ride " <> bppRideId.getId)
        return $ OUValidatedStopArrivedReq ValidatedStopArrivedReq {..}

mkBookingCancellationReason ::
  Id DRB.Booking ->
  Maybe (Id DRide.Ride) ->
  DBCR.CancellationSource ->
  Id DMerchant.Merchant ->
  DBCR.BookingCancellationReason
mkBookingCancellationReason bookingId mbRideId cancellationSource merchantId =
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
