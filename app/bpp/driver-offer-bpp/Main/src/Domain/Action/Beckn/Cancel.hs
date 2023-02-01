module Domain.Action.Beckn.Cancel
  ( cancel,
    CancelReq (..),
    CancelSearchReq (..),
    cancelSearch,
  )
where

import qualified Beckn.Storage.Esqueleto as DB
import qualified Beckn.Storage.Esqueleto as Esq
import Beckn.Storage.Hedis (HedisFlow)
import Beckn.Types.Common
import Beckn.Types.Id
import Beckn.Utils.Common
import Beckn.Utils.Servant.SignatureAuth (SignatureAuthResult (..))
import qualified Domain.Types.Booking as SRB
import qualified Domain.Types.BookingCancellationReason as DBCR
import qualified Domain.Types.Driver.DriverFlowStatus as DDFS
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Ride as SRide
import qualified Domain.Types.SearchRequest as SR
import EulerHS.Prelude
import qualified SharedLogic.CallBAP as BP
import qualified SharedLogic.DriverLocation as DLoc
import qualified SharedLogic.Ride as SRide
import Storage.CachedQueries.CacheConfig
import qualified Storage.CachedQueries.Merchant as QM
import qualified Storage.Queries.Booking as QRB
import qualified Storage.Queries.BookingCancellationReason as QBCR
import qualified Storage.Queries.Driver.DriverFlowStatus as QDFS
import qualified Storage.Queries.DriverInformation as QDI
import qualified Storage.Queries.Person as QPers
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.Ride as QRide
import Storage.Queries.SearchRequest as SR
import qualified Storage.Queries.SearchRequestForDriver as QSRD
import Tools.Error
import Tools.Metrics
import qualified Tools.Notifications as Notify

newtype CancelReq = CancelReq
  { bookingId :: Id SRB.Booking
  }

newtype CancelSearchReq = CancelSearchReq
  { searchId :: Id SR.SearchRequest
  }

cancel ::
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
    CoreMetrics m
  ) =>
  Id DM.Merchant ->
  SignatureAuthResult ->
  CancelReq ->
  m ()
cancel transporterId _ req = do
  transporter <-
    QM.findById transporterId
      >>= fromMaybeM (MerchantNotFound transporterId.getId)
  booking <- QRB.findById req.bookingId >>= fromMaybeM (BookingDoesNotExist req.bookingId.getId)
  let transporterId' = booking.providerId
  unless (transporterId' == transporterId) $ throwError AccessDenied
  mbRide <- QRide.findActiveByRBId req.bookingId
  bookingCR <- buildBookingCancellationReason
  Esq.runTransaction $ do
    QBCR.upsert bookingCR
    QRB.updateStatus booking.id SRB.CANCELLED
    whenJust mbRide $ \ride -> do
      QRide.updateStatus ride.id SRide.CANCELLED
      driverInfo <- QDI.findById (cast ride.driverId) >>= fromMaybeM (PersonNotFound ride.driverId.getId)
      if driverInfo.active
        then QDFS.updateStatus ride.driverId DDFS.ACTIVE
        else QDFS.updateStatus ride.driverId DDFS.IDLE
  whenJust mbRide $ \ride -> do
    SRide.clearCache $ cast ride.driverId
    DLoc.updateOnRide (cast ride.driverId) False

  logTagInfo ("bookingId-" <> getId req.bookingId) ("Cancellation reason " <> show bookingCR.source)
  fork "cancelBooking - Notify BAP" $ do
    BP.sendBookingCancelledUpdateToBAP booking transporter bookingCR.source
  whenJust mbRide $ \ride ->
    fork "cancelRide - Notify driver" $ do
      driver <- QPers.findById ride.driverId >>= fromMaybeM (PersonNotFound ride.driverId.getId)
      Notify.notifyOnCancel transporter.id booking driver.id driver.deviceToken bookingCR.source
  where
    buildBookingCancellationReason = do
      return $
        DBCR.BookingCancellationReason
          { bookingId = req.bookingId,
            rideId = Nothing,
            source = DBCR.ByUser,
            reasonCode = Nothing,
            driverId = Nothing,
            additionalInfo = Nothing,
            ..
          }

cancelSearch ::
  ( HasCacheConfig r,
    HedisFlow m r,
    EsqDBFlow m r,
    Esq.EsqDBReplicaFlow m r,
    CoreMetrics m
  ) =>
  Id DM.Merchant ->
  SignatureAuthResult ->
  CancelSearchReq ->
  m ()
cancelSearch transporterId _ req = do
  let transactionId = req.searchId
  searchID <- Esq.runInReplica $ SR.getRequestIdfromTransactionId transactionId >>= fromMaybeM (SearchRequestNotFound transactionId.getId)
  driverSearchReqs <- Esq.runInReplica $ QSRD.findAllActiveByRequestId searchID
  for_ driverSearchReqs $ \driverReq -> do
    logTagInfo ("searchId-" <> getId req.searchId) "Search Request Cancellation"
    DB.runTransaction $ do
      SR.updateStatus searchID SR.CANCELLED
      QSRD.setInactiveByRequestId driverReq.searchRequestId
    driver_ <- Esq.runInReplica $ QPerson.findById driverReq.driverId >>= fromMaybeM (PersonNotFound driverReq.driverId.getId)
    Notify.notifyOnCancelSearchRequest transporterId driverReq.driverId driver_.deviceToken driverReq.searchRequestId