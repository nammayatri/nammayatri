{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.Beckn.Cancel
  ( cancel,
    CancelReq (..),
    CancelSearchReq (..),
    validateCancelRequest,
    validateCancelSearchRequest,
    cancelSearch,
  )
where

import qualified Domain.Types.Booking as SRB
import qualified Domain.Types.BookingCancellationReason as DBCR
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Ride as SRide
import qualified Domain.Types.SearchRequest as DSR
import qualified Domain.Types.SearchTry as DST
import EulerHS.Prelude
import qualified Kernel.Storage.Esqueleto as Esq
import Kernel.Storage.Esqueleto.Config (EsqLocDBFlow, EsqLocRepDBFlow)
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.Common
import Kernel.Utils.Servant.SignatureAuth (SignatureAuthResult (..))
import Lib.SessionizerMetrics.Types.Event
import qualified SharedLogic.CallBAP as BP
import qualified SharedLogic.DriverLocation as DLoc
import qualified SharedLogic.DriverMode as DMode
import qualified SharedLogic.External.LocationTrackingService.Flow as LF
import qualified SharedLogic.External.LocationTrackingService.Types as LT
import qualified SharedLogic.SearchTryLocker as CS
import qualified Storage.CachedQueries.Merchant as QM
import qualified Storage.Queries.Booking as QRB
import qualified Storage.Queries.BookingCancellationReason as QBCR
import qualified Storage.Queries.Driver.DriverFlowStatus as QDFS
import qualified Storage.Queries.DriverInformation as QDI
import qualified Storage.Queries.DriverQuote as QDQ
import qualified Storage.Queries.Person as QPers
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.Ride as QRide
import qualified Storage.Queries.SearchRequest as QSR
import qualified Storage.Queries.SearchRequestForDriver as QSRD
import qualified Storage.Queries.SearchTry as QST
import Tools.Error
import Tools.Event
import qualified Tools.Notifications as Notify

newtype CancelReq = CancelReq
  { bookingId :: Id SRB.Booking
  }

newtype CancelSearchReq = CancelSearchReq
  { transactionId :: Text
  }

cancel ::
  ( EsqDBFlow m r,
    Esq.EsqDBReplicaFlow m r,
    EsqLocDBFlow m r,
    EsqLocRepDBFlow m r,
    CacheFlow m r,
    HasHttpClientOptions r c,
    EncFlow m r,
    HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    HasLongDurationRetryCfg r c,
    EventStreamFlow m r,
    LT.HasLocationService m r
  ) =>
  CancelReq ->
  DM.Merchant ->
  SRB.Booking ->
  m ()
cancel req merchant booking = do
  mbRide <- QRide.findActiveByRBId req.bookingId
  whenJust mbRide $ \ride -> do
    driverInfo <- QDI.findById (cast ride.driverId) >>= fromMaybeM (PersonNotFound ride.driverId.getId)
    DLoc.updateOnRide booking.providerId ride.driverId False
    QRide.updateStatus ride.id SRide.CANCELLED
    QDFS.updateStatus ride.driverId $ DMode.getDriverStatus driverInfo.mode driverInfo.active

  bookingCR <- buildBookingCancellationReason
  QBCR.upsert bookingCR
  QRB.updateStatus booking.id SRB.CANCELLED

  whenJust mbRide $ \ride -> do
    triggerRideCancelledEvent RideEventData {ride = ride{status = SRide.CANCELLED}, personId = ride.driverId, merchantId = merchant.id}
    triggerBookingCancelledEvent BookingEventData {booking = booking{status = SRB.CANCELLED}, personId = ride.driverId, merchantId = merchant.id}
    enableLocationTrackingService <- asks (.enableLocationTrackingService)
    when enableLocationTrackingService $
      void $ LF.rideDetails ride.id SRide.CANCELLED merchant.id ride.driverId booking.fromLocation.lat booking.fromLocation.lon

  logTagInfo ("bookingId-" <> getId req.bookingId) ("Cancellation reason " <> show bookingCR.source)
  fork "cancelBooking - Notify BAP" $ do
    BP.sendBookingCancelledUpdateToBAP booking merchant bookingCR.source
  whenJust mbRide $ \ride ->
    fork "cancelRide - Notify driver" $ do
      driver <- QPers.findById ride.driverId >>= fromMaybeM (PersonNotFound ride.driverId.getId)
      Notify.notifyOnCancel merchant.id booking driver.id driver.deviceToken bookingCR.source
  where
    buildBookingCancellationReason = do
      return $
        DBCR.BookingCancellationReason
          { bookingId = req.bookingId,
            rideId = Nothing,
            merchantId = Just booking.providerId,
            source = DBCR.ByUser,
            reasonCode = Nothing,
            driverId = Nothing,
            additionalInfo = Nothing,
            driverCancellationLocation = Nothing,
            driverDistToPickup = Nothing,
            ..
          }

cancelSearch ::
  ( CacheFlow m r,
    EsqDBFlow m r,
    Esq.EsqDBReplicaFlow m r
  ) =>
  Id DM.Merchant ->
  CancelSearchReq ->
  Id DSR.SearchRequest ->
  m ()
cancelSearch merchantId req searchRequestId = do
  searchTry <- QST.findActiveTryByRequestId searchRequestId >>= fromMaybeM (SearchTryDoesNotExist $ "searchRequestId-" <> searchRequestId.getId)
  CS.whenSearchTryCancellable searchTry.id $ do
    driverSearchReqs <- QSRD.findAllActiveBySRId searchRequestId
    logTagInfo ("transactionId-" <> req.transactionId) "Search Request Cancellation"
    _ <- QST.cancelActiveTriesByRequestId searchRequestId
    _ <- QSRD.setInactiveBySRId searchRequestId
    _ <- QDQ.setInactiveBySRId searchRequestId
    for_ driverSearchReqs $ \driverReq -> do
      driver_ <- QPerson.findById driverReq.driverId >>= fromMaybeM (PersonNotFound driverReq.driverId.getId)
      let searchTryId = fromMaybe (cast @DSR.SearchRequest @DST.SearchTry driverReq.requestId) driverReq.searchTryId -- FIXME use generic Search type
      Notify.notifyOnCancelSearchRequest merchantId driverReq.driverId driver_.deviceToken searchTryId

validateCancelSearchRequest ::
  ( EsqDBFlow m r
  ) =>
  Id DM.Merchant ->
  SignatureAuthResult ->
  CancelSearchReq ->
  m (Id DSR.SearchRequest)
validateCancelSearchRequest _ _ req = do
  let transactionId = req.transactionId
  QSR.findByTransactionId transactionId >>= fromMaybeM (SearchRequestNotFound $ "transactionId-" <> transactionId)

validateCancelRequest ::
  ( EsqDBFlow m r,
    CacheFlow m r
  ) =>
  Id DM.Merchant ->
  SignatureAuthResult ->
  CancelReq ->
  m (DM.Merchant, SRB.Booking)
validateCancelRequest merchantId _ req = do
  merchant <-
    QM.findById merchantId
      >>= fromMaybeM (MerchantNotFound merchantId.getId)
  booking <- QRB.findById req.bookingId >>= fromMaybeM (BookingDoesNotExist req.bookingId.getId)
  let merchantId' = booking.providerId
  unless (merchantId' == merchantId) $ throwError AccessDenied
  return (merchant, booking)
