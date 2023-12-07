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

import Data.Maybe (listToMaybe)
import Domain.Action.UI.Ride.CancelRide (driverDistanceToPickup)
import qualified Domain.Types.Booking as SRB
import qualified Domain.Types.BookingCancellationReason as DBCR
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Ride as SRide
import qualified Domain.Types.SearchTry as ST
import EulerHS.Prelude
import Kernel.External.Maps
import qualified Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.Common
import Kernel.Utils.Servant.SignatureAuth (SignatureAuthResult (..))
import qualified Lib.DriverCoins.Coins as DC
import qualified Lib.DriverCoins.Types as DCT
import Lib.SessionizerMetrics.Types.Event
import qualified SharedLogic.CallBAP as BP
import qualified SharedLogic.External.LocationTrackingService.Flow as LF
import qualified SharedLogic.External.LocationTrackingService.Types as LT
import qualified SharedLogic.SearchTryLocker as CS
import qualified Storage.CachedQueries.Driver.GoHomeRequest as CQDGR
import qualified Storage.CachedQueries.Merchant as QM
import qualified Storage.Queries.Booking as QRB
import qualified Storage.Queries.BookingCancellationReason as QBCR
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
    CacheFlow m r,
    HasHttpClientOptions r c,
    EncFlow m r,
    HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    HasLongDurationRetryCfg r c,
    EventStreamFlow m r,
    LT.HasLocationService m r,
    HasField "minTripDistanceForReferralCfg" r (Maybe HighPrecMeters)
  ) =>
  CancelReq ->
  DM.Merchant ->
  SRB.Booking ->
  m ()
cancel req merchant booking = do
  mbRide <- QRide.findActiveByRBId req.bookingId
  whenJust mbRide $ \ride -> do
    void $ CQDGR.setDriverGoHomeIsOnRideStatus ride.driverId booking.merchantOperatingCityId False
    QDI.updateOnRide (cast ride.driverId) False
    void $ LF.rideDetails ride.id SRide.CANCELLED merchant.id ride.driverId booking.fromLocation.lat booking.fromLocation.lon
    QRide.updateStatus ride.id SRide.CANCELLED

  bookingCR <- buildBookingCancellationReason
  QBCR.upsert bookingCR
  QRB.updateStatus booking.id SRB.CANCELLED

  fork "DriverRideCancelledCoin Location trakking" $ do
    whenJust mbRide $ \ride -> do
      mbLocation <- do
        driverLocations <- LF.driversLocation [ride.driverId]
        return $ listToMaybe driverLocations
      disToPickup <- forM mbLocation $ \location -> do
        driverDistanceToPickup booking.providerId booking.merchantOperatingCityId (getCoordinates location) (getCoordinates booking.fromLocation)
      logDebug $ "RideCancelled Coin Event by customer distance to pickup" <> show disToPickup
      logDebug "RideCancelled Coin Event by customer"
      DC.driverCoinsEvent ride.driverId merchant.id booking.merchantOperatingCityId (DCT.Cancellation ride.createdAt booking.distanceToPickup disToPickup)

  whenJust mbRide $ \ride -> do
    triggerRideCancelledEvent RideEventData {ride = ride{status = SRide.CANCELLED}, personId = ride.driverId, merchantId = merchant.id}
    triggerBookingCancelledEvent BookingEventData {booking = booking{status = SRB.CANCELLED}, personId = ride.driverId, merchantId = merchant.id}

  logTagInfo ("bookingId-" <> getId req.bookingId) ("Cancellation reason " <> show bookingCR.source)
  fork "cancelBooking - Notify BAP" $ do
    BP.sendBookingCancelledUpdateToBAP booking merchant bookingCR.source

  whenJust mbRide $ \ride ->
    fork "cancelRide - Notify driver" $ do
      driver <- QPers.findById ride.driverId >>= fromMaybeM (PersonNotFound ride.driverId.getId)
      Notify.notifyOnCancel booking.merchantOperatingCityId booking driver.id driver.deviceToken bookingCR.source
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
  ST.SearchTry ->
  m ()
cancelSearch _merchantId req searchTry = do
  CS.whenSearchTryCancellable searchTry.id $ do
    driverSearchReqs <- QSRD.findAllActiveBySRId searchTry.requestId
    logTagInfo ("transactionId-" <> req.transactionId) "Search Request Cancellation"
    _ <- QST.cancelActiveTriesByRequestId searchTry.requestId
    _ <- QSRD.setInactiveBySRId searchTry.requestId
    _ <- QDQ.setInactiveBySRId searchTry.requestId
    for_ driverSearchReqs $ \driverReq -> do
      driver_ <- QPerson.findById driverReq.driverId >>= fromMaybeM (PersonNotFound driverReq.driverId.getId)
      Notify.notifyOnCancelSearchRequest searchTry.merchantOperatingCityId driverReq.driverId driver_.deviceToken driverReq.searchTryId

validateCancelSearchRequest ::
  ( CacheFlow m r,
    EsqDBFlow m r
  ) =>
  Id DM.Merchant ->
  SignatureAuthResult ->
  CancelSearchReq ->
  m ST.SearchTry
validateCancelSearchRequest _ _ req = do
  let transactionId = req.transactionId
  searchReqId <- QSR.findByTransactionId transactionId >>= fromMaybeM (SearchRequestNotFound $ "transactionId-" <> transactionId)
  QST.findTryByRequestId searchReqId >>= fromMaybeM (SearchTryDoesNotExist $ "searchRequestId-" <> searchReqId.getId)

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
