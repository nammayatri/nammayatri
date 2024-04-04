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

import qualified BecknV2.OnDemand.Enums as Enums
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Domain.Types.Booking as SRB
import qualified Domain.Types.BookingCancellationReason as SBCR
import qualified Domain.Types.Merchant as DMerchant
import qualified Domain.Types.PersonFlowStatus as DPFS
import qualified Domain.Types.Ride as SRide
import Environment ()
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Sms.Config (SmsConfig)
import Kernel.Storage.Clickhouse.Config
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.SessionizerMetrics.Types.Event
import qualified SharedLogic.MerchantConfig as SMC
import qualified Storage.CachedQueries.BppDetails as CQBPP
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
    cancellationSource :: Maybe Text
  }

data ValidatedOnCancelReq = ValidatedBookingCancelledReq
  { bppBookingId :: Id SRB.BPPBooking,
    cancellationSource :: Maybe Text,
    booking :: SRB.Booking,
    mbRide :: Maybe SRide.Ride
  }

onCancel ::
  ( HasFlowEnv m r '["nwAddress" ::: BaseUrl, "smsCfg" ::: SmsConfig],
    KvDbFlow m r,
    EncFlow m r,
    EsqDBReplicaFlow m r,
    HasHttpClientOptions r c,
    HasLongDurationRetryCfg r c,
    HasField "minTripDistanceForReferralCfg" r (Maybe Distance),
    HasFlowEnv m r '["internalEndPointHashMap" ::: HM.HashMap BaseUrl BaseUrl],
    HasBAPMetrics m r,
    EventStreamFlow m r,
    HasField "hotSpotExpiry" r Seconds,
    ClickhouseFlow m r
  ) =>
  ValidatedOnCancelReq ->
  m ()
onCancel ValidatedBookingCancelledReq {..} = do
  let cancellationSource_ :: Maybe Enums.CancellationSource = readMaybe . T.unpack =<< cancellationSource
  logTagInfo ("BookingId-" <> getId booking.id) ""
  whenJust cancellationSource $ \source -> logTagInfo ("Cancellation source " <> source) ""
  merchantConfigs <- CMC.findAllByMerchantOperatingCityId booking.merchantOperatingCityId
  bookingCancellationReason <- mkBookingCancellationReason booking.id (mbRide <&> (.id)) (castCancellatonSource cancellationSource_) booking.merchantId
  fork "incrementing fraud counters" $ do
    let merchantOperatingCityId = booking.merchantOperatingCityId
    mFraudDetected <- SMC.anyFraudDetected booking.riderId merchantOperatingCityId merchantConfigs
    whenJust mFraudDetected $ \mc -> SMC.blockCustomer booking.riderId (Just mc.id)
  case mbRide of
    Just ride -> do
      case cancellationSource_ of
        Just Enums.CONSUMER -> SMC.updateCustomerFraudCounters booking.riderId merchantConfigs
        Just Enums.PROVIDER -> SMC.updateCancelledByDriverFraudCounters booking.riderId merchantConfigs
        _ -> pure ()
      triggerRideCancelledEvent RideEventData {ride = ride{status = SRide.CANCELLED}, personId = booking.riderId, merchantId = booking.merchantId}
    Nothing -> logDebug "No ride found for the booking."
  triggerBookingCancelledEvent BookingEventData {booking = booking{status = SRB.CANCELLED}}
  _ <- QPFS.updateStatus booking.riderId DPFS.IDLE
  unless (booking.status == SRB.CANCELLED) $ void $ QRB.updateStatus booking.id SRB.CANCELLED
  whenJust mbRide $ \ride -> void $ do
    unless (ride.status == SRide.CANCELLED) $ void $ QRide.updateStatus ride.id SRide.CANCELLED
  unless (cancellationSource_ == Just Enums.CONSUMER) $
    QBCR.upsert bookingCancellationReason
  QPFS.clearCache booking.riderId
  -- notify customer
  bppDetails <- CQBPP.findBySubscriberIdAndDomain booking.providerId Context.MOBILITY >>= fromMaybeM (InternalError $ "BPP details not found for providerId:- " <> booking.providerId <> "and domain:- " <> show Context.MOBILITY)
  Notify.notifyOnBookingCancelled booking (castCancellatonSource cancellationSource_) bppDetails
  where
    castCancellatonSource = \case
      Just Enums.CONSUMER -> SBCR.ByUser
      _ -> SBCR.ByDriver

validateRequest ::
  ( KvDbFlow m r,
    EsqDBReplicaFlow m r,
    HasHttpClientOptions r c,
    HasLongDurationRetryCfg r c,
    HasField "minTripDistanceForReferralCfg" r (Maybe Distance)
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
  (MonadFlow m) =>
  Id SRB.Booking ->
  Maybe (Id SRide.Ride) ->
  SBCR.CancellationSource ->
  Id DMerchant.Merchant ->
  m SBCR.BookingCancellationReason
mkBookingCancellationReason bookingId mbRideId cancellationSource merchantId = do
  now <- getCurrentTime
  return $
    SBCR.BookingCancellationReason
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
