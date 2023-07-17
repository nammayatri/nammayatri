{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.Beckn.Status
  ( handler,
    DStatusReq (..),
    DStatusRes (..),
    checkforSpecialZoneOtpExpiry,
  )
where

import Beckn.ACL.OnCancel (mkOnCancelMessage)
import Domain.Action.Beckn.Cancel as DCancel
import qualified Domain.Types.Booking as DBooking
import qualified Domain.Types.Booking as SRB
import qualified Domain.Types.BookingCancellationReason as DBCR
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Ride as DRide
import qualified Domain.Types.Ride as SRide
import EulerHS.Prelude
import Kernel.Beam.Functions as B
import Kernel.Storage.Esqueleto as Esq
import Kernel.Storage.Hedis (HedisFlow)
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Beckn.Context
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified SharedLogic.CallBAP as CallBAP
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.Queries.Booking as QRB
import qualified Storage.Queries.BookingCancellationReason as QBCR
import qualified Storage.Queries.DriverInformation as QDI
import qualified Storage.Queries.Person as QPers
import qualified Storage.Queries.Ride as QRide
import qualified Tools.Notifications as Notify
import qualified SharedLogic.External.LocationTrackingService.Flow as LF
import Tools.Event (RideEventData(..), BookingEventData (..), triggerRideCancelledEvent, triggerBookingCancelledEvent)
import SharedLogic.External.LocationTrackingService.Types
import Kernel.Streaming.Kafka.Producer.Types (KafkaProducerTools)
import Lib.SessionizerMetrics.Types.Event
import qualified SharedLogic.External.LocationTrackingService.Types as LT


newtype DStatusReq = StatusReq
  { bookingId :: Id DBooking.Booking
  }

data DStatusRes = StatusRes
  { transporter :: DM.Merchant,
    bookingId :: Id DBooking.Booking,
    bookingStatus :: DBooking.BookingStatus,
    mbRide :: Maybe DRide.Ride
  }

handler ::
  (CacheFlow m r, EsqDBFlow m r, EsqDBReplicaFlow m r) =>
  Id DM.Merchant ->
  DStatusReq ->
  m DStatusRes
handler transporterId req = do
  transporter <-
    CQM.findById transporterId
      >>= fromMaybeM (MerchantNotFound transporterId.getId)
  booking <- B.runInReplica $ QRB.findById req.bookingId >>= fromMaybeM (BookingNotFound req.bookingId.getId)
  mbRide <- B.runInReplica $ QRide.findOneByBookingId booking.id
  let transporterId' = booking.providerId
  unless (transporterId' == transporterId) $ throwError AccessDenied

  return $
    StatusRes
      { bookingId = booking.id,
        bookingStatus = booking.status,
        mbRide,
        transporter
      }

checkforSpecialZoneOtpExpiry ::
  ( HasCacheConfig r,
    HedisFlow m r,
    EsqDBFlow m r,
    Esq.EsqDBReplicaFlow m r,
    HedisFlow m r,
    CacheFlow m r,
    HasHttpClientOptions r c,
    EncFlow m r,
    HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    HasLongDurationRetryCfg r c,
    CoreMetrics m,
    HasFlowEnv m r '["ltsCfg" ::: LocationTrackingeServiceConfig],
    HasFlowEnv m r '["kafkaProducerTools" ::: KafkaProducerTools],
    HasShortDurationRetryCfg r c,
    EsqDBFlow m r,
    Esq.EsqDBReplicaFlow m r,
    CacheFlow m r,
    HasHttpClientOptions r c,
    EncFlow m r,
    HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    HasLongDurationRetryCfg r c,
    EventStreamFlow m r,
    LT.HasLocationService m r
  ) =>
  Id DM.Merchant ->
  Context ->
  DStatusReq ->
  m ()
checkforSpecialZoneOtpExpiry transporterId context req = do
  booking <- B.runInReplica $ QRB.findById req.bookingId >>= fromMaybeM (BookingNotFound req.bookingId.getId)
  now <- getCurrentTime
  when (booking.bookingType == DBooking.SpecialZoneBooking && Just now > booking.specialZoneOtpValidTill) $ do

    merchant <- CQM.findById transporterId >>= fromMaybeM (MerchantNotFound transporterId.getId)
    mbRide <- QRide.findActiveByRBId req.bookingId
    whenJust mbRide $ \ride -> do
      QDI.updateOnRide (cast ride.driverId) False
      void $ LF.rideDetails ride.id SRide.CANCELLED merchant.id ride.driverId booking.fromLocation.lat booking.fromLocation.lon
      QRide.updateStatus ride.id SRide.CANCELLED

    let merchantId = booking.providerId 
    bookingCR <- buildBookingCancellationReason merchantId
    QBCR.upsert bookingCR
    QRB.updateStatus booking.id SRB.CANCELLED
 
    whenJust mbRide $ \ride -> do
      triggerRideCancelledEvent RideEventData {ride = ride{status = SRide.CANCELLED}, personId = ride.driverId, merchantId = merchant.id}
      triggerBookingCancelledEvent BookingEventData {booking = booking{status = SRB.CANCELLED}, personId = ride.driverId, merchantId = merchant.id}
 
    logTagInfo ("bookingId-" <> getId req.bookingId) ("Cancellation reason " <> show bookingCR.source)
    -- fork "cancelBooking - Notify BAP" $ do
    --   BP.sendBookingCancelledUpdateToBAP booking merchant bookingCR.source
    whenJust mbRide $ \ride ->
      fork "cancelRide - Notify driver" $ do
        driver <- QPers.findById ride.driverId >>= fromMaybeM (PersonNotFound ride.driverId.getId)
        Notify.notifyOnCancel booking.merchantOperatingCityId booking driver.id driver.deviceToken bookingCR.source
    mbRideu <- B.runInReplica $ QRide.findActiveByRBId req.bookingId
    bookingu <- B.runInReplica $ QRB.findById req.bookingId >>= fromMaybeM (BookingNotFound req.bookingId.getId)
    let onCancelMessage = mkOnCancelMessage (buildOnCancelMessage merchant bookingu mbRideu bookingCR.source)
    void $ CallBAP.callOnCancel merchant context onCancelMessage
    void $ buildCancelRes mbRideu bookingCR.source bookingu merchant
 where
    buildBookingCancellationReason merchantId = do
      return $
        DBCR.BookingCancellationReason
          { bookingId = req.bookingId,
            rideId = Nothing,
            merchantId = Just merchantId,
            source = DBCR.ByUser,
            reasonCode = Nothing,
            driverId = Nothing,
            additionalInfo = Nothing,
            driverCancellationLocation = Nothing,
            driverDistToPickup = Nothing,
            ..
          }
    buildCancelRes mbRideu cancellationSource bookingu merchant = do
      return $
        CancelRes
          { transporter = merchant,
            bookingId = bookingu.id,
            bookingStatus = bookingu.status,
            cancellationSource = cancellationSource,
            mbRide = mbRideu
          }

    buildOnCancelMessage transporter bookingu mbRideu cancellationSource = do
      DCancel.CancelRes
        { transporter = transporter,
          bookingId = bookingu.id,
          bookingStatus = bookingu.status,
          cancellationSource = cancellationSource,
          mbRide = mbRideu
        }