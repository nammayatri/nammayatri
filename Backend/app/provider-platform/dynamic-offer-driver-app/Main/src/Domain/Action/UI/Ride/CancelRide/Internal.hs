{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.UI.Ride.CancelRide.Internal
  ( cancelRideImpl,
    updateNammaTagsForCancelledRide,
  )
where

import Data.Either.Extra (eitherToMaybe)
import qualified Data.HashMap.Strict as HM
import qualified Data.HashMap.Strict as HMS
import qualified Data.Map as M
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import qualified Domain.Types.Booking as SRB
import qualified Domain.Types.BookingCancellationReason as SBCR
import qualified Domain.Types.CancellationFarePolicy as DTC
import qualified Domain.Types.Merchant as DMerc
import qualified Domain.Types.Ride as DRide
import qualified Domain.Types.Yudhishthira as TY
import EulerHS.Prelude
import Kernel.Prelude hiding (any, elem)
import qualified Kernel.Storage.Esqueleto as Esq hiding (whenJust_)
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Streaming.Kafka.Producer.Types (KafkaProducerTools)
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.DriverScore as DS
import qualified Lib.DriverScore.Types as DST
import Lib.Scheduler (SchedulerType)
import Lib.SessionizerMetrics.Types.Event
import qualified Lib.Yudhishthira.Event as Yudhishthira
import qualified Lib.Yudhishthira.Types as Yudhishthira
import qualified SharedLogic.CallBAP as BP
import SharedLogic.Cancel
import qualified SharedLogic.External.LocationTrackingService.Flow as LF
import qualified SharedLogic.External.LocationTrackingService.Types as LT
import qualified SharedLogic.FareCalculator as FareCalculator
import SharedLogic.FarePolicy as SFP
import SharedLogic.GoogleTranslate (TranslateFlow)
import SharedLogic.Ride (updateOnRideStatusWithAdvancedRideCheck)
import qualified Storage.Cac.TransporterConfig as CTC
import qualified Storage.CachedQueries.Driver.GoHomeRequest as CQDGR
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.CachedQueries.ValueAddNP as CQVAN
import qualified Storage.Queries.Booking as QRB
import qualified Storage.Queries.BookingCancellationReason as QBCR
import qualified Storage.Queries.CallStatus as QCallStatus
import qualified Storage.Queries.DriverInformation as QDI
import qualified Storage.Queries.DriverStats as QDriverStats
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.Ride as QRide
import qualified Storage.Queries.RiderDetails as QRiderDetails
import qualified Storage.Queries.Vehicle as QVeh
import Tools.Constants
import Tools.Error
import Tools.Event
import qualified Tools.Metrics as Metrics
import qualified Tools.Notifications as Notify
import TransactionLogs.Types

cancelRideImpl ::
  ( MonadFlow m,
    EncFlow m r,
    EsqDBReplicaFlow m r,
    CacheFlow m r,
    EsqDBFlow m r,
    HasField "searchRequestExpirationSeconds" r NominalDiffTime,
    HasField "jobInfoMap" r (M.Map Text Bool),
    HasField "maxShards" r Int,
    HasField "schedulerSetName" r Text,
    HasField "schedulerType" r SchedulerType,
    Metrics.HasSendSearchRequestToDriverMetrics m r,
    HasFlowEnv m r '["kafkaProducerTools" ::: KafkaProducerTools],
    HasHttpClientOptions r c,
    HasLongDurationRetryCfg r c,
    HasField "singleBatchProcessingTempDelay" r NominalDiffTime,
    HasFlowEnv m r '["internalEndPointHashMap" ::: HM.HashMap BaseUrl BaseUrl],
    HasFlowEnv m r '["ondcTokenHashMap" ::: HMS.HashMap KeyConfig TokenConfig],
    HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    TranslateFlow m r,
    LT.HasLocationService m r,
    HasFlowEnv m r '["maxNotificationShards" ::: Int],
    HasShortDurationRetryCfg r c,
    Redis.HedisFlow m r,
    EventStreamFlow m r,
    Metrics.HasCoreMetrics r
  ) =>
  Id DRide.Ride ->
  DRide.RideEndedBy ->
  SBCR.BookingCancellationReason ->
  Bool ->
  Maybe Bool ->
  m ()
cancelRideImpl rideId rideEndedBy bookingCReason isForceReallocation doCancellationRateBasedBlocking = do
  ride <- QRide.findById rideId >>= fromMaybeM (RideDoesNotExist rideId.getId)
  booking <- QRB.findById ride.bookingId >>= fromMaybeM (BookingNotFound ride.bookingId.getId)
  isValueAddNP <- CQVAN.isValueAddNP booking.bapId
  let merchantId = booking.providerId
  merchant <-
    CQM.findById merchantId
      >>= fromMaybeM (MerchantNotFound merchantId.getId)
  transporterConfig <- CTC.findByMerchantOpCityId booking.merchantOperatingCityId Nothing >>= fromMaybeM (TransporterConfigNotFound booking.merchantOperatingCityId.getId)
  noShowCharges <- if transporterConfig.canAddCancellationFee then calculateNoShowCharges booking ride else return Nothing
  cancelRideTransaction booking ride bookingCReason merchantId rideEndedBy noShowCharges
  logTagInfo ("rideId-" <> getId rideId) ("Cancellation reason " <> show bookingCReason.source)
  driver <- QPerson.findById ride.driverId >>= fromMaybeM (PersonNotFound ride.driverId.getId)
  vehicle <- QVeh.findById ride.driverId >>= fromMaybeM (DriverWithoutVehicle ride.driverId.getId)
  fork "cancelRide - Notify driver" $ do
    rideTags <- updateNammaTagsForCancelledRide booking ride bookingCReason
    triggerRideCancelledEvent RideEventData {ride = ride{status = DRide.CANCELLED}, personId = driver.id, merchantId = merchantId}
    triggerBookingCancelledEvent BookingEventData {booking = booking{status = SRB.CANCELLED}, personId = driver.id, merchantId = merchantId}
    when (bookingCReason.source == SBCR.ByDriver) $
      DS.driverScoreEventHandler ride.merchantOperatingCityId DST.OnDriverCancellation {rideTags, merchantId = merchantId, driver = driver, rideFare = Just booking.estimatedFare, currency = booking.currency, distanceUnit = booking.distanceUnit, doCancellationRateBasedBlocking}
    Notify.notifyOnCancel ride.merchantOperatingCityId booking driver bookingCReason.source
  fork "cancelRide/ReAllocate - Notify BAP" $ do
    isReallocated <- reAllocateBookingIfPossible isValueAddNP False merchant booking ride driver vehicle bookingCReason isForceReallocation
    unless isReallocated $ BP.sendBookingCancelledUpdateToBAP booking merchant bookingCReason.source noShowCharges

calculateNoShowCharges :: (MonadFlow m, CacheFlow m r) => SRB.Booking -> DRide.Ride -> m (Maybe PriceAPIEntity)
calculateNoShowCharges booking ride = do
  mbFullFarePolicy <- SFP.getFarePolicyByEstOrQuoteIdWithoutFallback booking.quoteId
  let mbCancellationAndNoShowConfigs :: Maybe DTC.CancellationFarePolicy = (.cancellationFarePolicy) =<< mbFullFarePolicy
  now <- getCurrentTime
  logInfo $ "Params passed to calculateNoShowCharges: driverArrivalTime" <> show ride.driverArrivalTime <> " | cancellationAndNoShowConfigs: " <> show mbCancellationAndNoShowConfigs <> "| current time: " <> show now
  let cancellationCharges = FareCalculator.calculateNoShowCharges ride.driverArrivalTime mbCancellationAndNoShowConfigs now
  case cancellationCharges of
    Just cancellationFee -> do
      return $ Just PriceAPIEntity {amount = cancellationFee, currency = booking.currency}
    _ -> return Nothing

cancelRideTransaction ::
  ( EsqDBFlow m r,
    CacheFlow m r,
    Esq.EsqDBReplicaFlow m r,
    LT.HasLocationService m r
  ) =>
  SRB.Booking ->
  DRide.Ride ->
  SBCR.BookingCancellationReason ->
  Id DMerc.Merchant ->
  DRide.RideEndedBy ->
  Maybe PriceAPIEntity ->
  m ()
cancelRideTransaction booking ride bookingCReason merchantId rideEndedBy cancellationFee = do
  let driverId = cast ride.driverId
  void $ CQDGR.setDriverGoHomeIsOnRideStatus ride.driverId booking.merchantOperatingCityId False
  updateOnRideStatusWithAdvancedRideCheck driverId (Just ride)
  when booking.isScheduled $ QDI.updateLatestScheduledBookingAndPickup Nothing Nothing driverId
  void $ LF.rideDetails ride.id DRide.CANCELLED merchantId ride.driverId booking.fromLocation.lat booking.fromLocation.lon Nothing Nothing
  void $ QRide.updateStatus ride.id DRide.CANCELLED
  void $ QRide.updateRideEndedBy ride.id rideEndedBy
  QBCR.upsert bookingCReason
  void $ QRB.updateStatus booking.id SRB.CANCELLED
  when (bookingCReason.source == SBCR.ByDriver) $ QDriverStats.updateIdleTime driverId
  case (cancellationFee, booking.riderId) of
    (Just fee, Just rid) -> do
      QRide.updateCancellationFeeIfCancelledField (Just fee.amount) ride.id
      riderDetails <- QRiderDetails.findById rid >>= fromMaybeM (RiderDetailsNotFound rid.getId)
      QRiderDetails.updateCancellationDues (fee.amount + riderDetails.cancellationDues) rid
    _ -> do
      logError "cancelRideTransaction: riderId in booking or cancellationFee is not present"

updateNammaTagsForCancelledRide ::
  ( EsqDBFlow m r,
    CacheFlow m r,
    Esq.EsqDBReplicaFlow m r
  ) =>
  SRB.Booking ->
  DRide.Ride ->
  SBCR.BookingCancellationReason ->
  m [Text]
updateNammaTagsForCancelledRide booking ride bookingCReason = do
  now <- getCurrentTime
  mbCallStatus <- QCallStatus.findOneByEntityId (Just ride.id.getId)
  let callAtemptByDriver = isJust mbCallStatus
      currentTime = floor $ utcTimeToPOSIXSeconds now
      rideCreatedTime = floor $ utcTimeToPOSIXSeconds ride.createdAt
      driverArrivalTime = floor . utcTimeToPOSIXSeconds <$> (ride.driverArrivalTime)
      tagData =
        TY.CancelRideTagData
          { ride = ride{status = DRide.CANCELLED},
            booking = booking{status = SRB.CANCELLED},
            cancellationReason = bookingCReason,
            ..
          }
  nammaTags <- try @_ @SomeException (Yudhishthira.computeNammaTags Yudhishthira.RideCancel tagData)
  logDebug $ "Tags for cancelled ride, rideId: " <> ride.id.getId <> " tagresults:" <> show (eitherToMaybe nammaTags) <> "| tagdata: " <> show tagData
  let allTags = ride.rideTags <> eitherToMaybe nammaTags
  QRide.updateRideTags allTags ride.id
  driverStats <- QDriverStats.findById ride.driverId >>= fromMaybeM (PersonNotFound ride.driverId.getId)
  let tags = fromMaybe [] allTags
  when (validDriverCancellation `elem` tags) $ do
    QDriverStats.updateValidDriverCancellationTagCount (driverStats.validDriverCancellationTagCount + 1) ride.driverId
  when (validCustomerCancellation `elem` tags) $ do
    QDriverStats.updateValidCustomerCancellationTagCount (driverStats.validCustomerCancellationTagCount + 1) ride.driverId
  return $ fromMaybe [] allTags
