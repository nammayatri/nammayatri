{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE TypeApplications #-}

module Domain.Action.UI.Ride.CancelRide.Internal (cancelRideImpl) where

import qualified Domain.Types.Booking as SRB
import qualified Domain.Types.BookingCancellationReason as SBCR
import qualified Domain.Types.Common as DTC
import qualified Domain.Types.Merchant as DMerc
import qualified Domain.Types.Ride as DRide
import Environment
import Kernel.Prelude
import qualified Kernel.Storage.Esqueleto as Esq hiding (whenJust_)
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.DriverScore as DS
import qualified Lib.DriverScore.Types as DST
import SharedLogic.Allocator.Jobs.SendSearchRequestToDrivers (sendSearchRequestToDrivers')
import qualified SharedLogic.CallBAP as BP
import qualified SharedLogic.DriverPool as DP
import qualified SharedLogic.External.LocationTrackingService.Flow as LF
import qualified SharedLogic.External.LocationTrackingService.Types as LT
import SharedLogic.FarePolicy
import SharedLogic.SearchTry
import qualified Storage.CachedQueries.Driver.GoHomeRequest as CQDGR
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.CachedQueries.Merchant.TransporterConfig as QTC
import qualified Storage.Queries.Booking as QRB
import qualified Storage.Queries.BookingCancellationReason as QBCR
import qualified Storage.Queries.DriverInformation as QDI
import qualified Storage.Queries.DriverQuote as QDQ
import qualified Storage.Queries.DriverStats as QDriverStats
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.Ride as QRide
import qualified Storage.Queries.SearchRequest as QSR
import qualified Storage.Queries.SearchTry as QST
import Tools.Error
import Tools.Event
import qualified Tools.Notifications as Notify

cancelRideImpl :: Id DRide.Ride -> SBCR.BookingCancellationReason -> Flow ()
cancelRideImpl rideId bookingCReason = do
  ride <- QRide.findById rideId >>= fromMaybeM (RideDoesNotExist rideId.getId)
  booking <- QRB.findById ride.bookingId >>= fromMaybeM (BookingNotFound ride.bookingId.getId)
  let merchantId = booking.providerId
  merchant <-
    CQM.findById merchantId
      >>= fromMaybeM (MerchantNotFound merchantId.getId)
  cancelRideTransaction booking ride bookingCReason merchantId
  logTagInfo ("rideId-" <> getId rideId) ("Cancellation reason " <> show bookingCReason.source)

  fork "cancelRide - Notify driver" $ do
    driver <- QPerson.findById ride.driverId >>= fromMaybeM (PersonNotFound ride.driverId.getId)
    triggerRideCancelledEvent RideEventData {ride = ride{status = DRide.CANCELLED}, personId = driver.id, merchantId = merchantId}
    triggerBookingCancelledEvent BookingEventData {booking = booking{status = SRB.CANCELLED}, personId = driver.id, merchantId = merchantId}
    when (bookingCReason.source == SBCR.ByDriver) $
      DS.driverScoreEventHandler ride.merchantOperatingCityId DST.OnDriverCancellation {merchantId = merchantId, driverId = driver.id, rideFare = Just booking.estimatedFare}
    Notify.notifyOnCancel ride.merchantOperatingCityId booking driver.id driver.deviceToken bookingCReason.source

  fork "cancelRide - Notify BAP" $ do
    case booking.tripCategory of
      DTC.OneWay DTC.OneWayOnDemandDynamicOffer -> do
        -- in this case only do the reallocation
        driverQuote <- QDQ.findById (Id booking.quoteId) >>= fromMaybeM (QuoteNotFound booking.quoteId)
        searchTry <- QST.findById driverQuote.searchTryId >>= fromMaybeM (SearchTryNotFound driverQuote.searchTryId.getId)
        searchReq <- QSR.findById searchTry.requestId >>= fromMaybeM (SearchRequestNotFound searchTry.requestId.getId)
        transpConf <- QTC.findByMerchantOpCityId searchReq.merchantOperatingCityId >>= fromMaybeM (TransporterConfigNotFound searchReq.merchantOperatingCityId.getId)
        let searchRepeatLimit = transpConf.searchRepeatLimit
        now <- getCurrentTime
        let isSearchTryValid = searchTry.validTill > now
        driverReachedDistance <- highPrecMetersToMeters <$> asks (.driverReachedDistance)
        let driverHasNotArrived = isNothing ride.driverArrivalTime || maybe True (> driverReachedDistance) bookingCReason.driverDistToPickup
        let isRepeatSearch =
              searchTry.searchRepeatCounter < searchRepeatLimit
                && bookingCReason.source == SBCR.ByDriver
                && isSearchTryValid
                && fromMaybe False searchReq.isReallocationEnabled
                && driverHasNotArrived
        if isRepeatSearch
          then do
            void $ addDriverToSearchCancelledList searchReq.id ride
            result <- try @_ @SomeException (initiateDriverSearchBatch sendSearchRequestToDrivers' merchant searchReq driverQuote.tripCategory searchTry.vehicleVariant searchTry.estimateId searchTry.customerExtraFee searchTry.messageId)
            case result of
              Right _ -> BP.sendEstimateRepetitionUpdateToBAP booking ride (Id searchTry.estimateId) bookingCReason.source
              Left _ -> cancelRideTransactionForNonReallocation booking (Just searchTry.estimateId) merchant bookingCReason.source
          else -- repeatSearch merchant farePolicy searchReq searchTry booking ride SBCR.ByDriver now driverPoolCfg
            cancelRideTransactionForNonReallocation booking (Just searchTry.estimateId) merchant bookingCReason.source
      _ -> cancelRideTransactionForNonReallocation booking Nothing merchant bookingCReason.source
  where
    cancelRideTransactionForNonReallocation booking mbEstimateId merchant bookingCancellationReason = do
      whenJust mbEstimateId $ \estimateId ->
        void $ clearCachedFarePolicyByEstOrQuoteId estimateId
      void $ clearCachedFarePolicyByEstOrQuoteId booking.quoteId
      BP.sendBookingCancelledUpdateToBAP booking merchant bookingCancellationReason

    addDriverToSearchCancelledList searchReqId ride = do
      let keyForDriverCancelledList = DP.mkBlockListedDriversKey searchReqId
      cacheBlockListedDrivers keyForDriverCancelledList ride.driverId

    cacheBlockListedDrivers key driverId = do
      searchRequestExpirationSeconds <- asks (.searchRequestExpirationSeconds)
      Redis.withCrossAppRedis $ Redis.rPushExp key [driverId] (round searchRequestExpirationSeconds)

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
  m ()
cancelRideTransaction booking ride bookingCReason merchantId = do
  let driverId = cast ride.driverId
  void $ CQDGR.setDriverGoHomeIsOnRideStatus ride.driverId booking.merchantOperatingCityId False
  QDI.updateOnRide (cast ride.driverId) False
  void $ LF.rideDetails ride.id DRide.CANCELLED merchantId ride.driverId booking.fromLocation.lat booking.fromLocation.lon
  void $ QRide.updateStatus ride.id DRide.CANCELLED
  QBCR.upsert bookingCReason
  void $ QRB.updateStatus booking.id SRB.CANCELLED
  when (bookingCReason.source == SBCR.ByDriver) $ QDriverStats.updateIdleTime driverId
