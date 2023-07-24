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

import qualified Domain.Types.Booking as SRB
import qualified Domain.Types.BookingCancellationReason as SBCR
import qualified Domain.Types.Merchant as DMerchant
import qualified Domain.Types.Person.PersonFlowStatus as DPFS
import qualified Domain.Types.Ride as SRide
import EulerHS.Prelude hiding (id)
import qualified Kernel.Storage.Esqueleto as DB
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import Kernel.Storage.Esqueleto.Transactionable (runInReplica)
import Kernel.Storage.Hedis
import Kernel.Types.Common hiding (id)
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.SessionizerMetrics.Types.Event
import qualified SharedLogic.MerchantConfig as SMC
import Storage.CachedQueries.CacheConfig
import qualified Storage.CachedQueries.MerchantConfig as CMC
import qualified Storage.CachedQueries.Person.PersonFlowStatus as QPFS
import qualified Storage.Queries.Booking as QRB
import qualified Storage.Queries.BookingCancellationReason as QBCR
import qualified Storage.Queries.Ride as QRide
import Tools.Error
import Tools.Event
import Tools.Metrics
import qualified Tools.Notifications as Notify

data OnCancelReq = OnCancelReq
  { bppBookingId :: Id SRB.BPPBooking,
    cancellationSource :: SBCR.CancellationSource
  }

data ValidatedOnCancelBookingReq = ValidatedOnCancelBookingReq
  { bppBookingId :: Id SRB.BPPBooking,
    cancellationSource :: SBCR.CancellationSource,
    booking :: SRB.Booking,
    mbRide :: Maybe SRide.Ride
  }

onCancel ::
  ( HasCacheConfig r,
    MonadFlow m,
    EsqDBReplicaFlow m r,
    EncFlow m r,
    CoreMetrics m,
    EventStreamFlow m r,
    HedisFlow m r,
    EsqDBFlow m r
  ) =>
  ValidatedOnCancelBookingReq ->
  m ()
onCancel ValidatedOnCancelBookingReq {..} = do
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

validateRequest :: (EsqDBFlow m r, EsqDBReplicaFlow m r) => OnCancelReq -> m ValidatedOnCancelBookingReq
validateRequest OnCancelReq {..} = do
  booking <- runInReplica $ QRB.findByBPPBookingId bppBookingId >>= fromMaybeM (BookingDoesNotExist $ "BppBookingId: " <> bppBookingId.getId)
  mbRide <- QRide.findActiveByRBId booking.id
  let isRideCancellable = maybe False (\ride -> ride.status `notElem` [SRide.INPROGRESS, SRide.CANCELLED]) mbRide
      bookingAlreadyCancelled = booking.status == SRB.CANCELLED
  unless (isBookingCancellable booking || (isRideCancellable && bookingAlreadyCancelled)) $
    throwError (BookingInvalidStatus (show booking.status))
  return $ ValidatedOnCancelBookingReq {..}
  where
    isBookingCancellable booking =
      booking.status `elem` [SRB.NEW, SRB.CONFIRMED, SRB.AWAITING_REASSIGNMENT, SRB.TRIP_ASSIGNED]

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
