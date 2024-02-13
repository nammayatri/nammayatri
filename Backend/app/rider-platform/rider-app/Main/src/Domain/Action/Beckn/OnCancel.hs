{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wwarn=incomplete-record-updates #-}

module Domain.Action.Beckn.OnCancel
  ( onCancel,
    validateRequest,
    OnCancelReq (..),
  )
where

import qualified Data.HashMap.Strict as HM
import qualified Domain.Types.Booking as SRB
import qualified Domain.Types.BookingCancellationReason as SBCR
import qualified Domain.Types.Merchant as DMerchant
import qualified Domain.Types.Person.PersonFlowStatus as DPFS
import qualified Domain.Types.Ride as SRide
import Environment ()
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Sms.Config (SmsConfig)
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.SessionizerMetrics.Types.Event
import qualified SharedLogic.MerchantConfig as SMC
import qualified Storage.CachedQueries.MerchantConfig as CMC
import qualified Storage.CachedQueries.Person.PersonFlowStatus as QPFS
import qualified Storage.Queries.Booking as QRB
import qualified Storage.Queries.BookingCancellationReason as QBCR
import qualified Storage.Queries.Ride as QRide
import Tools.Error
import Tools.Event
import Tools.Metrics (HasBAPMetrics)
import qualified Tools.Notifications as Notify

data OnCancelReq = BookingCancelledReq
  { bppBookingId :: Id SRB.BPPBooking,
    cancellationSource :: SBCR.CancellationSource
  }

data ValidatedOnCancelReq = ValidatedBookingCancelledReq
  { bppBookingId :: Id SRB.BPPBooking,
    cancellationSource :: SBCR.CancellationSource,
    booking :: SRB.Booking,
    mbRide :: Maybe SRide.Ride
  }

onCancel ::
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
  ValidatedOnCancelReq ->
  m ()
onCancel ValidatedBookingCancelledReq {..} = do
  logTagInfo ("BookingId-" <> getId booking.id) ("Cancellation reason " <> show cancellationSource)
  let bookingCancellationReason = mkBookingCancellationReason booking.id (mbRide <&> (.id)) cancellationSource booking.merchantId
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
  _ <- QPFS.updateStatus booking.riderId DPFS.IDLE
  unless (booking.status == SRB.CANCELLED) $ void $ QRB.updateStatus booking.id SRB.CANCELLED
  whenJust mbRide $ \ride -> void $ do
    unless (ride.status == SRide.CANCELLED) $ void $ QRide.updateStatus ride.id SRide.CANCELLED
  unless (cancellationSource == SBCR.ByUser) $
    QBCR.upsert bookingCancellationReason
  QPFS.clearCache booking.riderId
  -- notify customer
  Notify.notifyOnBookingCancelled booking cancellationSource

validateRequest ::
  ( CacheFlow m r,
    EsqDBFlow m r,
    EsqDBReplicaFlow m r,
    HasHttpClientOptions r c,
    HasLongDurationRetryCfg r c,
    HasField "minTripDistanceForReferralCfg" r (Maybe HighPrecMeters)
  ) =>
  OnCancelReq ->
  m ValidatedOnCancelReq
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
