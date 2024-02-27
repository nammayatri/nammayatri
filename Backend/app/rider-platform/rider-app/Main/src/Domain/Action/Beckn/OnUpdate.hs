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
    ProviderInfo (..),
    EstimateInfo (..),
    QuoteInfo (..),
    OneWayQuoteDetails (..),
    OneWaySpecialZoneQuoteDetails (..),
    QuoteDetails (..),
    InterCityQuoteDetails (..),
  )
where

import qualified Data.HashMap.Strict as HM
import Data.Time hiding (getCurrentTime)
import qualified Domain.Action.Beckn.Common as Common
import qualified Domain.Types.Booking as SRB
import qualified Domain.Types.BookingCancellationReason as SBCR
import qualified Domain.Types.Estimate as DEstimate
import qualified Domain.Types.EstimateRevised as DER
import qualified Domain.Types.Merchant as DMerchant
import qualified Domain.Types.Person.PersonFlowStatus as DPFS
import qualified Domain.Types.QuoteRevised as DQR
import qualified Domain.Types.Ride as SRide
import Domain.Types.SearchRequest
import qualified Domain.Types.SearchRequest as DSR
import qualified Domain.Types.SearchRequest as DSearchReq
import qualified Domain.Types.SpecialZoneQuote as DSpecialZoneQuote
import qualified Domain.Types.TripTerms as DTripTerms
import Domain.Types.VehicleVariant
import Environment ()
import Kernel.Beam.Functions
import qualified Kernel.External.Maps.Types as Maps
import Kernel.Prelude
import Kernel.Sms.Config (SmsConfig)
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.SessionizerMetrics.Types.Event
import qualified Storage.CachedQueries.Merchant as QMerch
import qualified Storage.CachedQueries.Person.PersonFlowStatus as QPFS
import qualified Storage.Queries.Booking as QRB
import qualified Storage.Queries.BookingCancellationReason as QBCR
import qualified Storage.Queries.Estimate as QEstimate
import qualified Storage.Queries.EstimateRevised as QER
import qualified Storage.Queries.QuoteRevised as QQR
import qualified Storage.Queries.Ride as QRide
import qualified Storage.Queries.SearchRequest as QSR
import qualified Storage.Queries.SearchRequest as QSearchReq
import Tools.Error
import Tools.Event
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
  | UpdatedEstimateReq
      { requestId :: Id DSearchReq.SearchRequest,
        providerInfo :: ProviderInfo,
        estimateInfo :: [EstimateInfo],
        quoteInfo :: [QuoteInfo]
      }

data ValidatedOnUpdateReq
  = ValidatedRideAssignedReq Common.RideAssignedReq
  | ValidatedRideStartedReq Common.RideStartedReq
  | ValidatedRideCompletedReq Common.RideCompletedReq
  | ValidatedBookingCancelledReq Common.BookingCancelledReq
  | ValidatedBookingReallocationReq
      { bppBookingId :: Id SRB.BPPBooking,
        bppRideId :: Id SRide.BPPRide,
        reallocationSource :: SBCR.CancellationSource,
        booking :: SRB.Booking,
        ride :: SRide.Ride
      }
  | ValidatedDriverArrivedReq Common.DriverArrivedReq
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
  | ValidatedUpdatedEstimateReq
      { requestId :: Id DSR.SearchRequest,
        providerInfo :: ProviderInfo,
        estimateInfo :: [EstimateInfo],
        quoteInfo :: [QuoteInfo],
        _searchRequest :: SearchRequest,
        merchant :: DMerchant.Merchant
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
    nightShiftEnd :: TimeOfDay,
    oldNightShiftCharge :: Maybe Centesimal
  }

data WaitingChargesInfo = WaitingChargesInfo
  -- { waitingTimeEstimatedThreshold :: Maybe Seconds,
  { waitingChargePerMin :: Maybe Money
  }

data EstimateBreakupInfo = EstimateBreakupInfo
  { title :: Text,
    price :: BreakupPriceInfo
  }

data BreakupPriceInfo = BreakupPriceInfo
  { currency :: Text,
    value :: Money
  }

data ProviderInfo = ProviderInfo
  { providerId :: Text,
    name :: Text,
    url :: BaseUrl,
    mobileNumber :: Text,
    ridesCompleted :: Int
  }

data EstimateInfo = EstimateInfo
  { bppEstimateRevisedId :: Id DER.BPPEstimateRevised,
    vehicleVariant :: VehicleVariant,
    estimatedFare :: Money,
    discount :: Maybe Money,
    estimatedTotalFare :: Money,
    itemId :: Text,
    totalFareRange :: DER.FareRange,
    descriptions :: [Text],
    estimateBreakupList :: [EstimateBreakupInfo],
    nightShiftInfo :: Maybe NightShiftInfo,
    -- waitingCharges :: Maybe WaitingChargesInfo,
    specialLocationTag :: Maybe Text
  }

data QuoteInfo = QuoteInfo
  { vehicleVariant :: VehicleVariant,
    estimatedFare :: Money,
    discount :: Maybe Money,
    estimatedTotalFare :: Money,
    quoteDetails :: QuoteDetails,
    itemId :: Text,
    descriptions :: [Text],
    specialLocationTag :: Maybe Text
  }

data QuoteDetails
  = OneWayDetails OneWayQuoteDetails
  | OneWaySpecialZoneDetails OneWaySpecialZoneQuoteDetails
  | InterCityDetails InterCityQuoteDetails

newtype OneWayQuoteDetails = OneWayQuoteDetails
  { distanceToNearestDriver :: HighPrecMeters
  }

newtype OneWaySpecialZoneQuoteDetails = OneWaySpecialZoneQuoteDetails
  { quoteId :: Text
  }

newtype InterCityQuoteDetails = InterCityQuoteDetails
  { quoteId :: Text
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
  ValidatedUpdatedEstimateReq {..} -> do
    now <- getCurrentTime
    estimateRevised <- traverse (buildEstimateRevised providerInfo now _searchRequest) estimateInfo
    quoteRevised <- traverse (buildQuoteRevised requestId providerInfo now _searchRequest) quoteInfo
    forM_ estimateRevised $ \est -> do
      triggerEstimateRevisedEvent EstimateRevisedEventData {estimateRevised = est, personId = _searchRequest.riderId, merchantId = _searchRequest.merchantId}
    -- QER.createMany estimateRevised
    case listToMaybe estimateRevised of
      Just est -> do
        QER.create est
      Nothing -> throwError (InvalidRequest $ "No estimate recieved")
    QQR.createMany quoteRevised

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
    return $ ValidatedRideAssignedReq req
  OURideStartedReq req -> do
    return $ ValidatedRideStartedReq req
  OURideCompletedReq req -> do
    return $ ValidatedRideCompletedReq req
  OUBookingCancelledReq req -> do
    return $ ValidatedBookingCancelledReq req
  BookingReallocationReq {..} -> do
    booking <- runInReplica $ QRB.findByBPPBookingId bppBookingId >>= fromMaybeM (BookingDoesNotExist $ "BppBookingId:-" <> bppBookingId.getId)
    ride <- QRide.findByBPPRideId bppRideId >>= fromMaybeM (RideDoesNotExist $ "BppRideId" <> bppRideId.getId)
    return $ ValidatedBookingReallocationReq {..}
  OUDriverArrivedReq req -> do
    return $ ValidatedDriverArrivedReq req
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
  UpdatedEstimateReq {..} -> do
    _searchRequest <- runInReplica $ QSearchReq.findById requestId >>= fromMaybeM (SearchRequestDoesNotExist requestId.getId)
    merchant <- QMerch.findById _searchRequest.merchantId >>= fromMaybeM (MerchantNotFound _searchRequest.merchantId.getId)
    return $ ValidatedUpdatedEstimateReq {..}

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

buildEstimateRevised ::
  MonadFlow m =>
  ProviderInfo ->
  UTCTime ->
  SearchRequest ->
  EstimateInfo ->
  m DER.EstimateRevised
buildEstimateRevised providerInfo now _searchRequest EstimateInfo {..} = do
  uid <- generateGUID
  tripTerms <- buildTripTerms descriptions
  estimateBreakupList' <- buildEstimateBreakUp estimateBreakupList uid
  pure
    DER.EstimateRevised
      { id = uid,
        requestId = _searchRequest.id,
        merchantId = Just _searchRequest.merchantId,
        merchantOperatingCityId = Just _searchRequest.merchantOperatingCityId,
        providerMobileNumber = providerInfo.mobileNumber,
        providerName = providerInfo.name,
        providerCompletedRidesCount = providerInfo.ridesCompleted,
        providerId = providerInfo.providerId,
        providerUrl = providerInfo.url,
        estimatedDistance = _searchRequest.distance,
        estimatedDuration = _searchRequest.estimatedRideDuration,
        device = _searchRequest.device,
        createdAt = now,
        updatedAt = now,
        status = DER.NEW,
        estimateRevisedBreakupList = estimateBreakupList',
        driversLocation = [Maps.LatLong {lat = 0, lon = 0}],
        nightShiftInfo =
          nightShiftInfo <&> \nightShiftInfo' ->
            DER.NightShiftInfo
              { nightShiftCharge = nightShiftInfo'.nightShiftCharge,
                oldNightShiftCharge = nightShiftInfo'.oldNightShiftCharge, -- TODO: Doesn't make sense, to be removed
                nightShiftStart = nightShiftInfo'.nightShiftStart,
                nightShiftEnd = nightShiftInfo'.nightShiftEnd
              },
        waitingCharges =
          DER.WaitingCharges
            { -- { waitingChargePerMin = waitingCharges >>= (.waitingChargePerMin)
              -- },
              waitingChargePerMin = Nothing
            },
        ..
      }

buildQuoteRevised ::
  MonadFlow m =>
  Id DSearchReq.SearchRequest ->
  ProviderInfo ->
  UTCTime ->
  SearchRequest ->
  QuoteInfo ->
  m DQR.QuoteRevised
buildQuoteRevised requestId providerInfo now _searchRequest QuoteInfo {..} = do
  logDebug $ "hello world onUpdate quote build start"
  uid <- generateGUID
  tripTerms <- buildTripTerms descriptions
  quoteDetails' <- case quoteDetails of
    OneWayDetails oneWayDetails ->
      pure.DQR.OneWayDetails $ mkOneWayQuoteDetails oneWayDetails
    OneWaySpecialZoneDetails details -> do
      DQR.OneWaySpecialZoneDetails <$> buildOneWaySpecialZoneQuoteDetails details
    InterCityDetails details -> do
      DQR.InterCityDetails <$> buildInterCityQuoteDetails details
  logDebug $ "hello world onUpdate quote build end"
  pure
    DQR.QuoteRevised
      { id = uid,
        providerId = providerInfo.providerId,
        providerUrl = providerInfo.url,
        createdAt = now,
        quoteRevisedDetails = quoteDetails',
        merchantId = _searchRequest.merchantId,
        merchantOperatingCityId = _searchRequest.merchantOperatingCityId,
        ..
      }

mkOneWayQuoteDetails :: OneWayQuoteDetails -> DQR.OneWayQuoteRevisedDetails
mkOneWayQuoteDetails OneWayQuoteDetails {..} = DQR.OneWayQuoteRevisedDetails {..}

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
  Id DER.EstimateRevised ->
  m [DER.EstimateRevisedBreakup]
buildEstimateBreakUp estimatesItems estId =
  estimatesItems
    `for` \estimateItem -> do
      id <- generateGUID
      price' <- mkEstimatePrice estimateItem.price
      pure
        DER.EstimateRevisedBreakup
          { title = estimateItem.title,
            price = price',
            estimateRevisedId = estId,
            ..
          }

mkEstimatePrice ::
  MonadFlow m =>
  BreakupPriceInfo ->
  m DER.EstimateRevisedBreakupPrice
mkEstimatePrice BreakupPriceInfo {..} = pure DER.EstimateRevisedBreakupPrice {..}

buildOneWaySpecialZoneQuoteDetails :: MonadFlow m => OneWaySpecialZoneQuoteDetails -> m DSpecialZoneQuote.SpecialZoneQuote
buildOneWaySpecialZoneQuoteDetails OneWaySpecialZoneQuoteDetails {..} = do
  id <- generateGUID
  pure DSpecialZoneQuote.SpecialZoneQuote {..}

buildInterCityQuoteDetails :: MonadFlow m => InterCityQuoteDetails -> m DSpecialZoneQuote.SpecialZoneQuote
buildInterCityQuoteDetails InterCityQuoteDetails {..} = do
  id <- generateGUID
  pure DSpecialZoneQuote.SpecialZoneQuote {..}
