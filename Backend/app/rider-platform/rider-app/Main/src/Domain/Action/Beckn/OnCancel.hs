{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.Beckn.OnCancel
  ( onCancel,
    validateRequest,
    OnCancelReq (..),
  )
where

import qualified Beckn.ACL.Select as ACL
import qualified Data.HashMap as HM
import qualified Domain.Action.UI.Select as DSelect
import qualified Domain.Types.Booking as SRB
import qualified Domain.Types.BookingCancellationReason as SBCR
import qualified Domain.Types.Estimate as DEstimate
import qualified Domain.Types.Merchant as DMerchant
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Person.PersonFlowStatus as DPFS
import qualified Domain.Types.Ride as SRide
import qualified Domain.Types.SearchRequest as DSR
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import qualified Kernel.Storage.Hedis as Redis
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.SessionizerMetrics.Types.Event
import qualified SharedLogic.CallBPP as CallBPP
import qualified SharedLogic.MerchantConfig as SMC
import qualified Storage.CachedQueries.Merchant as QM
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.CachedQueries.MerchantConfig as CMC
import qualified Storage.CachedQueries.Person.PersonFlowStatus as QPFS
import qualified Storage.Queries.Booking as QRB
import qualified Storage.Queries.BookingCancellationReason as QBCR
import qualified Storage.Queries.Estimate as QEstimate
import qualified Storage.Queries.Ride as QRide
import qualified Storage.Queries.SearchRequest as QSR
import Tools.Error
import Tools.Event
import Tools.Metrics (HasBAPMetrics)
import qualified Tools.Notifications as Notify

data OnCancelReq = OnCancelReq
  { searchRequestId :: Id DSR.SearchRequest,
    bppEstimateId :: Id DEstimate.BPPEstimate,
    bppBookingId :: Id SRB.BPPBooking,
    bppRideId :: Id SRide.BPPRide,
    cancellationSource :: SBCR.CancellationSource,
    isOldEstimateValid :: Bool
  }

data ValidatedOnCancelReq = ValidatedOnCancelReq
  { cancellationSource :: SBCR.CancellationSource,
    booking :: SRB.Booking,
    mbRide :: Maybe SRide.Ride,
    searchReq :: DSR.SearchRequest,
    estimate :: DEstimate.Estimate,
    isOldEstimateValid :: Bool
  }

onCancel ::
  ( HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    CacheFlow m r,
    EsqDBFlow m r,
    MonadFlow m,
    EncFlow m r,
    EsqDBReplicaFlow m r,
    HasHttpClientOptions r c,
    HasLongDurationRetryCfg r c,
    HasField "shortDurationRetryCfg" r RetryCfg,
    HasFlowEnv m r '["internalEndPointHashMap" ::: HM.Map BaseUrl BaseUrl],
    HasBAPMetrics m r,
    EventStreamFlow m r
  ) =>
  ValidatedOnCancelReq ->
  m ()
onCancel ValidatedOnCancelReq {..} = do
  let bookingCancellationReason = mkBookingCancellationReason booking.id (mbRide <&> (.id)) cancellationSource booking.merchantId
  whenJust mbRide $ \ride -> void $ do
    unless (ride.status == SRide.CANCELLED) $ void $ QRide.updateStatus ride.id SRide.CANCELLED
  unless (cancellationSource == SBCR.ByUser) $
    QBCR.upsert bookingCancellationReason
  if isOldEstimateValid
    then callEstimateReallocation searchReq estimate booking
    else callBookingCancellation booking mbRide cancellationSource

callEstimateReallocation ::
  ( HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    CacheFlow m r,
    EsqDBFlow m r,
    MonadFlow m,
    EncFlow m r,
    EsqDBReplicaFlow m r,
    HasHttpClientOptions r c,
    HasLongDurationRetryCfg r c,
    HasField "shortDurationRetryCfg" r RetryCfg,
    HasFlowEnv m r '["internalEndPointHashMap" ::: HM.Map BaseUrl BaseUrl],
    HasBAPMetrics m r
  ) =>
  DSR.SearchRequest ->
  DEstimate.Estimate ->
  SRB.Booking ->
  m ()
callEstimateReallocation searchReq estimate booking = do
  logTagInfo ("EstimateId-" <> getId estimate.id) "Estimate repetition."
  void $ QPFS.updateStatus searchReq.riderId DPFS.WAITING_FOR_DRIVER_OFFERS {estimateId = estimate.id, validTill = searchReq.validTill}
  QPFS.clearCache searchReq.riderId
  let merchantOperatingCityId = searchReq.merchantOperatingCityId
  merchant <- QM.findById searchReq.merchantId >>= fromMaybeM (MerchantNotFound searchReq.merchantId.getId)
  city <- CQMOC.findById merchantOperatingCityId >>= fmap (.city) . fromMaybeM (MerchantOperatingCityNotFound merchantOperatingCityId.getId)
  -- notify customer
  Notify.notifyOnEstimatedReallocated booking estimate.id
  Redis.whenWithLockRedis (selectEstimateLockKey searchReq.riderId) 60 $ do
    void $ QEstimate.updateStatus estimate.id DEstimate.DRIVER_QUOTE_REQUESTED
    void $ QRB.updateStatus booking.id SRB.REALLOCATED
    let dSelectRes = mkSelectRes searchReq.customerExtraFee searchReq estimate merchant city
    becknReq <- ACL.buildSelectReq dSelectRes
    logTagInfo "Sending reallocated select request to bpp " $ show becknReq
    void $ withShortRetry $ CallBPP.select dSelectRes.providerUrl becknReq
  where
    mkSelectRes :: Maybe Money -> DSR.SearchRequest -> DEstimate.Estimate -> DMerchant.Merchant -> Context.City -> DSelect.DSelectRes
    mkSelectRes customerExtraFee searchRequest estimate' merchant city =
      DSelect.DSelectRes
        { estimate = estimate',
          providerId = estimate'.providerId,
          providerUrl = estimate'.providerUrl,
          variant = estimate'.vehicleVariant,
          autoAssignEnabled = True,
          isOldEstimateValid = True,
          ..
        }

callBookingCancellation ::
  ( HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    CacheFlow m r,
    EsqDBFlow m r,
    MonadFlow m,
    EncFlow m r,
    EsqDBReplicaFlow m r,
    HasHttpClientOptions r c,
    HasLongDurationRetryCfg r c,
    HasField "shortDurationRetryCfg" r RetryCfg,
    HasBAPMetrics m r,
    EventStreamFlow m r
  ) =>
  SRB.Booking ->
  Maybe SRide.Ride ->
  SBCR.CancellationSource ->
  m ()
callBookingCancellation booking mbRide cancellationSource = do
  logTagInfo ("BookingId-" <> getId booking.id) ("Cancellation reason " <> show cancellationSource)
  merchantConfigs <- CMC.findAllByMerchantOperatingCityId booking.merchantOperatingCityId
  case cancellationSource of
    SBCR.ByUser -> SMC.updateCustomerFraudCounters booking.riderId merchantConfigs
    SBCR.ByDriver -> SMC.updateCancelledByDriverFraudCounters booking.riderId merchantConfigs
    _ -> pure ()
  fork "incrementing fraud counters" $ do
    let merchantOperatingCityId = booking.merchantOperatingCityId
    mFraudDetected <- SMC.anyFraudDetected booking.riderId merchantOperatingCityId merchantConfigs
    whenJust mFraudDetected $ \mc -> SMC.blockCustomer booking.riderId (Just mc.id)
  case mbRide of
    Just ride -> do
      triggerRideCancelledEvent RideEventData {ride = ride{status = SRide.CANCELLED}, personId = booking.riderId, merchantId = booking.merchantId}
    Nothing -> do
      logDebug "No ride found for the booking."
  triggerBookingCancelledEvent BookingEventData {booking = booking{status = SRB.CANCELLED}}
  void $ QPFS.updateStatus booking.riderId DPFS.IDLE
  unless (booking.status == SRB.CANCELLED) $ void $ QRB.updateStatus booking.id SRB.CANCELLED
  QPFS.clearCache booking.riderId
  -- notify customer
  Notify.notifyOnBookingCancelled booking cancellationSource

selectEstimateLockKey :: Id DP.Person -> Text
selectEstimateLockKey personId = "Customer:SelectEstimate:CustomerId-" <> personId.getId

validateRequest ::
  ( CacheFlow m r,
    EsqDBFlow m r,
    EsqDBReplicaFlow m r,
    HasHttpClientOptions r c,
    HasLongDurationRetryCfg r c
  ) =>
  OnCancelReq ->
  m ValidatedOnCancelReq
validateRequest OnCancelReq {..} = do
  booking <- runInReplica $ QRB.findByBPPBookingId bppBookingId >>= fromMaybeM (BookingDoesNotExist $ "BppBookingId: " <> bppBookingId.getId)
  searchReq <- QSR.findById searchRequestId >>= fromMaybeM (SearchRequestNotFound searchRequestId.getId)
  mbRide <- QRide.findActiveByRBId booking.id
  let isRideCancellable = maybe False (\ride -> ride.status `notElem` [SRide.INPROGRESS, SRide.CANCELLED]) mbRide
      bookingAlreadyCancelled = booking.status == SRB.CANCELLED
  unless (isBookingCancellable booking || (isRideCancellable && bookingAlreadyCancelled)) $
    throwError (BookingInvalidStatus (show booking.status))
  estimate <- QEstimate.findByBPPEstimateId bppEstimateId >>= fromMaybeM (EstimateDoesNotExist bppEstimateId.getId)
  return $ ValidatedOnCancelReq {..}
  where
    isBookingCancellable booking =
      booking.status `elem` [SRB.NEW, SRB.CONFIRMED, SRB.AWAITING_REASSIGNMENT, SRB.TRIP_ASSIGNED]

mkBookingCancellationReason ::
  Id SRB.Booking ->
  Maybe (Id SRide.Ride) ->
  SBCR.CancellationSource ->
  Id DMerchant.Merchant ->
  SBCR.BookingCancellationReason
mkBookingCancellationReason bookingId mbRideId cancellationSource merchantId =
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
