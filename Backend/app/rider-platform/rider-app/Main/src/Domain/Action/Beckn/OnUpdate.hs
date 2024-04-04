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
    QuoteRepetitionReq (..),
    NewMessageReq (..),
    SafetyAlertReq (..),
    StopArrivedReq (..),
    ValidatedOnUpdateReq (..),
    EditDestSoftUpdateReq (..),
    EditDestConfirmUpdateReq (..),
  )
where

import qualified Data.HashMap.Strict as HM
import Data.Time hiding (getCurrentTime)
import qualified Domain.Action.Beckn.Common as Common
import qualified Domain.Types.Booking as DRB
import qualified Domain.Types.BookingCancellationReason as DBCR
import qualified Domain.Types.BookingUpdateRequest as DBUR
import qualified Domain.Types.Estimate as DEstimate
import qualified Domain.Types.Extra.Booking as SRB
import qualified Domain.Types.FareBreakup as DFareBreakup
import qualified Domain.Types.LocationMapping as DLM
import qualified Domain.Types.Merchant as DMerchant
import qualified Domain.Types.PersonFlowStatus as DPFS
import qualified Domain.Types.Ride as DRide
import qualified Domain.Types.SearchRequest as DSR
import Domain.Types.VehicleVariant
import Environment ()
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Sms.Config (SmsConfig)
import Kernel.Storage.Clickhouse.Config
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.SessionizerMetrics.Types.Event
import qualified SharedLogic.LocationMapping as SLM
import qualified Storage.CachedQueries.Person.PersonFlowStatus as QPFS
import qualified Storage.Queries.Booking as QRB
import qualified Storage.Queries.BookingCancellationReason as QBCR
import qualified Storage.Queries.BookingUpdateRequest as QBUR
import qualified Storage.Queries.Estimate as QEstimate
import qualified Storage.Queries.FareBreakup as QFareBreakup
import qualified Storage.Queries.LocationMapping as QLM
import qualified Storage.Queries.Quote as SQQ
import qualified Storage.Queries.Ride as QRide
import qualified Storage.Queries.SearchRequest as QSR
import qualified Storage.Queries.Transformers.Booking as STB
import Tools.Error
import Tools.Maps (LatLong)
import Tools.Metrics (HasBAPMetrics)
import qualified Tools.Notifications as Notify
import TransactionLogs.Types

data OnUpdateReq
  = OURideAssignedReq Common.RideAssignedReq
  | OURideStartedReq Common.RideStartedReq
  | OURideCompletedReq Common.RideCompletedReq
  | OUBookingCancelledReq Common.BookingCancelledReq
  | OUBookingReallocationReq BookingReallocationReq -- not used
  | OUDriverArrivedReq Common.DriverArrivedReq
  | OUEstimateRepetitionReq EstimateRepetitionReq
  | OUQuoteRepetitionReq QuoteRepetitionReq
  | OUNewMessageReq NewMessageReq
  | OUSafetyAlertReq SafetyAlertReq
  | OUStopArrivedReq StopArrivedReq
  | OUEditDestSoftUpdateReq EditDestSoftUpdateReq
  | OUEditDestConfirmUpdateReq EditDestConfirmUpdateReq

data ValidatedOnUpdateReq
  = OUValidatedRideAssignedReq Common.ValidatedRideAssignedReq
  | OUValidatedRideStartedReq Common.ValidatedRideStartedReq
  | OUValidatedRideCompletedReq Common.ValidatedRideCompletedReq
  | OUValidatedFarePaidReq Common.ValidatedFarePaidReq
  | OUValidatedBookingCancelledReq Common.ValidatedBookingCancelledReq
  | OUValidatedBookingReallocationReq ValidatedBookingReallocationReq
  | OUValidatedDriverArrivedReq Common.ValidatedDriverArrivedReq
  | OUValidatedEstimateRepetitionReq ValidatedEstimateRepetitionReq
  | OUValidatedQuoteRepetitionReq ValidatedQuoteRepetitionReq
  | OUValidatedNewMessageReq ValidatedNewMessageReq
  | OUValidatedSafetyAlertReq ValidatedSafetyAlertReq
  | OUValidatedStopArrivedReq ValidatedStopArrivedReq
  | OUValidatedEditDestSoftUpdateReq ValidatedEditDestSoftUpdateReq
  | OUValidatedEditDestConfirmUpdateReq ValidatedEditDestConfirmUpdateReq

data BookingReallocationReq = BookingReallocationReq
  { bppBookingId :: Id DRB.BPPBooking,
    bppRideId :: Id DRide.BPPRide,
    reallocationSource :: DBCR.CancellationSource
  }

data EditDestSoftUpdateReq = EditDestSoftUpdateReq
  { bookingDetails :: Common.BookingDetails,
    fare :: Price,
    fareBreakups :: [Common.DFareBreakup],
    newEstimatedDistance :: HighPrecMeters,
    currentPoint :: Maybe LatLong,
    bookingUpdateRequestId :: Id DBUR.BookingUpdateRequest
  }

data EditDestConfirmUpdateReq = EditDestConfirmUpdateReq
  { bookingDetails :: Common.BookingDetails,
    bookingUpdateRequestId :: Id DBUR.BookingUpdateRequest
  }

data ValidatedEditDestSoftUpdateReq = ValidatedEditDestSoftUpdateReq
  { bookingDetails :: Common.BookingDetails,
    fare :: Price,
    fareBreakups :: [Common.DFareBreakup],
    newEstimatedDistance :: HighPrecMeters,
    currentPoint :: Maybe LatLong,
    bookingUpdateRequestId :: Id DBUR.BookingUpdateRequest,
    booking :: DRB.Booking,
    ride :: DRide.Ride,
    bookingUpdateRequest :: DBUR.BookingUpdateRequest
  }

data ValidatedEditDestConfirmUpdateReq = ValidatedEditDestConfirmUpdateReq
  { bookingDetails :: Common.BookingDetails,
    bookingUpdateRequestId :: Id DBUR.BookingUpdateRequest,
    booking :: DRB.Booking,
    ride :: DRide.Ride,
    bookingUpdateRequest :: DBUR.BookingUpdateRequest
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

data QuoteRepetitionReq = QuoteRepetitionReq
  { searchRequestId :: Id DSR.SearchRequest,
    newBppBookingId :: Id DRB.BPPBooking,
    bppBookingId :: Id DRB.BPPBooking,
    bppRideId :: Id DRide.BPPRide,
    cancellationSource :: DBCR.CancellationSource
  }

data ValidatedQuoteRepetitionReq = ValidatedQuoteRepetitionReq
  { searchRequestId :: Id DSR.SearchRequest,
    newBppBookingId :: Id DRB.BPPBooking,
    bppBookingId :: Id DRB.BPPBooking,
    bppRideId :: Id DRide.BPPRide,
    cancellationSource :: DBCR.CancellationSource,
    booking :: DRB.Booking,
    ride :: DRide.Ride,
    searchReq :: DSR.SearchRequest
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
    ClickhouseFlow m r,
    -- HasShortDurationRetryCfg r c, -- uncomment for test update api
    HasField "minTripDistanceForReferralCfg" r (Maybe Distance),
    HasFlowEnv m r '["internalEndPointHashMap" ::: HM.HashMap BaseUrl BaseUrl],
    HasFlowEnv m r '["ondcTokenHashMap" ::: HM.HashMap KeyConfig TokenConfig],
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
    bookingCancellationReason <- mkBookingCancellationReason booking.id (mbRide <&> (.id)) reallocationSource booking.merchantId
    void $ QRB.updateStatus booking.id DRB.AWAITING_REASSIGNMENT
    void $ QRide.updateStatus ride.id DRide.CANCELLED
    QBCR.upsert bookingCancellationReason
    Notify.notifyOnBookingReallocated booking
  OUValidatedDriverArrivedReq req -> Common.driverArrivedReqHandler req
  OUValidatedNewMessageReq ValidatedNewMessageReq {..} -> Notify.notifyOnNewMessage booking message
  OUValidatedEstimateRepetitionReq ValidatedEstimateRepetitionReq {..} -> do
    when (cancellationSource /= DBCR.ByUser) $ do
      -- in case cancellation is by user, we don't need to create a new booking cancellation reason as already created in the previous step
      bookingCancellationReason <- mkBookingCancellationReason booking.id (Just ride.id) cancellationSource booking.merchantId
      void $ QBCR.upsert bookingCancellationReason
    logTagInfo ("EstimateId-" <> getId estimate.id) "Estimate repetition."

    void $ QEstimate.updateStatus DEstimate.DRIVER_QUOTE_REQUESTED estimate.id
    void $ QRB.updateStatus booking.id DRB.REALLOCATED
    void $ QRide.updateStatus ride.id DRide.CANCELLED
    void $ QPFS.updateStatus searchReq.riderId DPFS.WAITING_FOR_DRIVER_OFFERS {estimateId = estimate.id, otherSelectedEstimates = Nothing, validTill = searchReq.validTill}
    QPFS.clearCache searchReq.riderId
    -- notify customer
    Notify.notifyOnEstOrQuoteReallocated cancellationSource booking estimate.id.getId
  OUValidatedQuoteRepetitionReq ValidatedQuoteRepetitionReq {..} -> do
    when (cancellationSource /= DBCR.ByUser) $ do
      -- in case cancellation is by user, we don't need to create a new booking cancellation reason as already created in the previous step
      bookingCancellationReason <- mkBookingCancellationReason booking.id (Just ride.id) cancellationSource booking.merchantId
      void $ QBCR.upsert bookingCancellationReason

    quote <- case booking.quoteId of
      Just quoteId -> SQQ.findById quoteId >>= fromMaybeM (QuoteNotFound quoteId.getId)
      _ -> throwError $ InvalidRequest ("Quote not found for bookingId: " <> booking.id.getId)
    now <- getCurrentTime
    bookingId <- generateGUID
    quoteId_ <- generateGUID
    let newQuote = quote{id = Id quoteId_, createdAt = now, updatedAt = now}
        newBooking = booking{id = bookingId, quoteId = Just (Id quoteId_), status = SRB.CONFIRMED, isScheduled = False, bppBookingId = Just newBppBookingId, startTime = booking.startTime, createdAt = now, updatedAt = now}
    void $ SQQ.createQuote newQuote
    void $ QRB.createBooking newBooking
    void $ QRB.updateStatus booking.id DRB.REALLOCATED
    void $ QRide.updateStatus ride.id DRide.CANCELLED
    void $ QPFS.updateStatus searchReq.riderId DPFS.WAITING_FOR_DRIVER_ASSIGNMENT {bookingId = bookingId, validTill = searchReq.validTill, fareProductType = Just $ STB.getFareProductType booking.bookingDetails}
    QPFS.clearCache searchReq.riderId
    -- notify customer
    Notify.notifyOnEstOrQuoteReallocated cancellationSource booking quote.id.getId
  OUValidatedSafetyAlertReq ValidatedSafetyAlertReq {..} -> Notify.notifySafetyAlert booking code
  OUValidatedStopArrivedReq ValidatedStopArrivedReq {..} -> do
    QRB.updateStop booking Nothing
    Notify.notifyOnStopReached booking ride
  OUValidatedEditDestSoftUpdateReq ValidatedEditDestSoftUpdateReq {..} -> do
    let currentPointLat = (.lat) <$> currentPoint
        currentPointLon = (.lon) <$> currentPoint
    breakups <- traverse (Common.buildFareBreakupV2 bookingUpdateRequestId.getId DFareBreakup.BOOKING_UPDATE_REQUEST) fareBreakups
    QFareBreakup.createMany breakups
    QBUR.updateMultipleById Nothing (Just newEstimatedDistance) (Just fare.amount) Nothing currentPointLat currentPointLon bookingUpdateRequestId
  OUValidatedEditDestConfirmUpdateReq ValidatedEditDestConfirmUpdateReq {..} -> do
    dropLocMapping <- QLM.getLatestEndByEntityId bookingUpdateRequest.id.getId >>= fromMaybeM (InternalError $ "Latest drop location mapping not found for bookingUpdateRequestId: " <> bookingUpdateRequest.id.getId)
    dropLocMap <- SLM.buildDropLocationMapping dropLocMapping.locationId booking.id.getId DLM.BOOKING (Just bookingUpdateRequest.merchantId) (Just bookingUpdateRequest.merchantOperatingCityId)
    QLM.create dropLocMap
    estimatedFare <- bookingUpdateRequest.estimatedFare & fromMaybeM (InternalError "Estimated fare not found for bookingUpdateRequestId")
    QRB.updateMultipleById estimatedFare estimatedFare bookingUpdateRequest.estimatedDistance bookingUpdateRequest.bookingId

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
  OUQuoteRepetitionReq QuoteRepetitionReq {..} -> do
    booking <- runInReplica $ QRB.findByBPPBookingId bppBookingId >>= fromMaybeM (BookingDoesNotExist $ "BppBookingId:-" <> bppBookingId.getId)
    searchReq <- QSR.findById searchRequestId >>= fromMaybeM (SearchRequestNotFound searchRequestId.getId)
    ride <- QRide.findByBPPRideId bppRideId >>= fromMaybeM (RideDoesNotExist $ "BppRideId" <> bppRideId.getId)
    return $ OUValidatedQuoteRepetitionReq ValidatedQuoteRepetitionReq {..}
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
  OUEditDestSoftUpdateReq EditDestSoftUpdateReq {..} -> do
    let Common.BookingDetails {..} = bookingDetails
    booking <- runInReplica $ QRB.findByBPPBookingId bppBookingId >>= fromMaybeM (BookingDoesNotExist $ "BppBookingId:-" <> bppBookingId.getId)
    ride <- runInReplica $ QRide.findByBPPRideId bppRideId >>= fromMaybeM (RideDoesNotExist $ "BppRideId" <> bppRideId.getId)
    when (ride.status == DRide.COMPLETED) $ throwError $ RideInvalidStatus "Can't edit the destination of a completed ride."
    bookingUpdateRequest <- runInReplica $ QBUR.findById bookingUpdateRequestId >>= fromMaybeM (InternalError $ "BookingUpdateRequest not found with Id:-" <> bookingUpdateRequestId.getId)
    return $ OUValidatedEditDestSoftUpdateReq ValidatedEditDestSoftUpdateReq {..}
  OUEditDestConfirmUpdateReq EditDestConfirmUpdateReq {..} -> do
    let Common.BookingDetails {..} = bookingDetails
    booking <- runInReplica $ QRB.findByBPPBookingId bppBookingId >>= fromMaybeM (BookingDoesNotExist $ "BppBookingId:-" <> bppBookingId.getId)
    ride <- runInReplica $ QRide.findByBPPRideId bppRideId >>= fromMaybeM (RideDoesNotExist $ "BppRideId" <> bppRideId.getId)
    when (ride.status == DRide.COMPLETED) $ throwError $ RideInvalidStatus "Can't edit the destination of a completed ride."
    bookingUpdateRequest <- runInReplica $ QBUR.findById bookingUpdateRequestId >>= fromMaybeM (InternalError $ "BookingUpdateRequest not found with Id:-" <> bookingUpdateRequestId.getId)
    return $ OUValidatedEditDestConfirmUpdateReq ValidatedEditDestConfirmUpdateReq {..}

mkBookingCancellationReason ::
  (MonadFlow m) =>
  Id DRB.Booking ->
  Maybe (Id DRide.Ride) ->
  DBCR.CancellationSource ->
  Id DMerchant.Merchant ->
  m DBCR.BookingCancellationReason
mkBookingCancellationReason bookingId mbRideId cancellationSource merchantId = do
  now <- getCurrentTime
  return $
    DBCR.BookingCancellationReason
      { bookingId = bookingId,
        rideId = mbRideId,
        merchantId = Just merchantId,
        source = cancellationSource,
        reasonCode = Nothing,
        reasonStage = Nothing,
        additionalInfo = Nothing,
        driverCancellationLocation = Nothing,
        driverDistToPickup = Nothing,
        createdAt = now,
        updatedAt = now
      }
