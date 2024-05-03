{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.UI.Ride.CancelRide.Internal (cancelRideImpl) where

import qualified Domain.Types.Booking as SRB
import qualified Domain.Types.BookingCancellationReason as SBCR
import qualified Domain.Types.Common as DTC
import qualified Domain.Types.Merchant as DMerc
import qualified Domain.Types.Ride as DRide
import Environment
import Kernel.Prelude
import qualified Kernel.Storage.Esqueleto as Esq hiding (whenJust_)
import Kernel.Storage.Hedis as Redis
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.DriverScore as DS
import qualified Lib.DriverScore.Types as DST
import SharedLogic.Allocator.Jobs.SendSearchRequestToDrivers (sendSearchRequestToDrivers')
import qualified SharedLogic.CallBAP as BP
import SharedLogic.DriverPool
import qualified SharedLogic.DriverPool as DP
import qualified SharedLogic.External.LocationTrackingService.Flow as LF
import qualified SharedLogic.External.LocationTrackingService.Types as LT
import SharedLogic.FarePolicy
import SharedLogic.Ride (multipleRouteKey, searchRequestKey)
import SharedLogic.SearchTry
import qualified Storage.CachedQueries.Driver.GoHomeRequest as CQDGR
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.CachedQueries.Merchant.TransporterConfig as QTC
import qualified Storage.CachedQueries.ValueAddNP as CQVAN
import qualified Storage.Queries.Booking as QRB
import qualified Storage.Queries.BookingCancellationReason as QBCR
import qualified Storage.Queries.DriverInformation as QDI
import qualified Storage.Queries.DriverQuote as QDQ
import qualified Storage.Queries.DriverStats as QDriverStats
import qualified Storage.Queries.Estimate as QEst
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.Quote as QQuote
import qualified Storage.Queries.Ride as QRide
import qualified Storage.Queries.SearchRequest as QSR
import qualified Storage.Queries.SearchTry as QST
import qualified Storage.Queries.Vehicle as QVeh
import Tools.Error
import Tools.Event
import qualified Tools.Notifications as Notify

cancelRideImpl :: Id DRide.Ride -> DRide.RideEndedBy -> SBCR.BookingCancellationReason -> Flow ()
cancelRideImpl rideId rideEndedBy bookingCReason = do
  ride <- QRide.findById rideId >>= fromMaybeM (RideDoesNotExist rideId.getId)
  booking <- QRB.findById ride.bookingId >>= fromMaybeM (BookingNotFound ride.bookingId.getId)
  isValueAddNP <- CQVAN.isValueAddNP booking.bapId
  let merchantId = booking.providerId
  merchant <-
    CQM.findById merchantId
      >>= fromMaybeM (MerchantNotFound merchantId.getId)
  cancelRideTransaction booking ride bookingCReason merchantId rideEndedBy
  logTagInfo ("rideId-" <> getId rideId) ("Cancellation reason " <> show bookingCReason.source)
  driver <- QPerson.findById ride.driverId >>= fromMaybeM (PersonNotFound ride.driverId.getId)
  vehicle <- QVeh.findById ride.driverId >>= fromMaybeM (DriverWithoutVehicle ride.driverId.getId)
  now <- getCurrentTime

  fork "cancelRide - Notify driver" $ do
    triggerRideCancelledEvent RideEventData {ride = ride{status = DRide.CANCELLED}, personId = driver.id, merchantId = merchantId}
    triggerBookingCancelledEvent BookingEventData {booking = booking{status = SRB.CANCELLED}, personId = driver.id, merchantId = merchantId}
    when (bookingCReason.source == SBCR.ByDriver) $
      DS.driverScoreEventHandler ride.merchantOperatingCityId DST.OnDriverCancellation {merchantId = merchantId, driverId = driver.id, rideFare = Just booking.estimatedFare}
    Notify.notifyOnCancel ride.merchantOperatingCityId booking driver.id driver.deviceToken bookingCReason.source

  fork "cancelRide - Notify BAP" $ do
    case booking.tripCategory of
      DTC.OneWay DTC.OneWayOnDemandDynamicOffer -> do
        driverQuote <- QDQ.findById (Id booking.quoteId) >>= fromMaybeM (QuoteNotFound booking.quoteId)
        searchTry <- QST.findById driverQuote.searchTryId >>= fromMaybeM (SearchTryNotFound driverQuote.searchTryId.getId)
        searchReq <- QSR.findById searchTry.requestId >>= fromMaybeM (SearchRequestNotFound searchTry.requestId.getId)
        isRepeatSearch <- checkIfRepeatSearch searchTry booking ride.driverArrivalTime searchReq.isReallocationEnabled now
        if isRepeatSearch
          then do
            estimate <- QEst.findById driverQuote.estimateId >>= fromMaybeM (EstimateNotFound driverQuote.estimateId.getId)
            DP.addDriverToSearchCancelledList searchReq.id ride.driverId
            tripQuoteDetail <- buildTripQuoteDetail searchReq booking.tripCategory booking.vehicleServiceTier estimate.vehicleServiceTierName (estimate.minFare + fromMaybe 0 searchTry.customerExtraFee) (Just 0) (Just $ estimate.maxFare - estimate.minFare) estimate.driverPickUpCharge estimate.id.getId
            let driverSearchBatchInput =
                  DriverSearchBatchInput
                    { sendSearchRequestToDrivers = sendSearchRequestToDrivers',
                      merchant,
                      searchReq,
                      tripQuoteDetails = [tripQuoteDetail],
                      customerExtraFee = searchTry.customerExtraFee,
                      messageId = searchTry.messageId,
                      isRepeatSearch
                    }
            result <- try @_ @SomeException (initiateDriverSearchBatch driverSearchBatchInput)
            case result of
              Right _ -> do
                if isValueAddNP
                  then BP.sendEstimateRepetitionUpdateToBAP booking ride (Id searchTry.estimateId) bookingCReason.source driver vehicle
                  else cancelRideTransactionForNonReallocation Nothing booking (Just searchTry.estimateId) merchant bookingCReason.source
              Left _ -> cancelRideTransactionForNonReallocation Nothing booking (Just searchTry.estimateId) merchant bookingCReason.source
          else cancelRideTransactionForNonReallocation Nothing booking (Just searchTry.estimateId) merchant bookingCReason.source
      DTC.Rental DTC.OnDemandStaticOffer -> do
        quote <- QQuote.findById (Id booking.quoteId) >>= fromMaybeM (QuoteNotFound booking.quoteId)
        searchReq <- QSR.findById quote.searchRequestId >>= fromMaybeM (SearchRequestNotFound quote.searchRequestId.getId)
        searchTry <- QST.findLastByRequestId quote.searchRequestId >>= fromMaybeM (SearchTryNotFound quote.searchRequestId.getId)
        isRepeatSearch <- checkIfRepeatSearch searchTry booking ride.driverArrivalTime searchReq.isReallocationEnabled now
        if isRepeatSearch
          then do
            DP.addDriverToSearchCancelledList searchReq.id ride.driverId
            bookingId <- generateGUID
            quoteId <- generateGUID
            fareParamsId <- generateGUID
            searchRequestExpirationSeconds <- asks (.searchRequestExpirationSeconds)
            let newFareParams = quote.fareParams{id = fareParamsId, updatedAt = now}
                newQuote = quote{id = Id quoteId, fareParams = newFareParams, validTill = searchRequestExpirationSeconds `addUTCTime` now, isScheduled = False} -- check if validTill req'D
                newBooking = booking{id = bookingId, quoteId = quoteId, status = SRB.NEW, isScheduled = False, startTime = max now booking.startTime, createdAt = now, updatedAt = now}
            tripQuoteDetail <- buildTripQuoteDetail searchReq booking.tripCategory booking.vehicleServiceTier quote.vehicleServiceTierName booking.estimatedFare quote.driverMinFee quote.driverMaxFee quote.driverPickUpCharge newQuote.id.getId
            void $ clearCachedFarePolicyByEstOrQuoteId booking.quoteId
            QQuote.create newQuote
            QRB.createBooking newBooking
            let driverSearchBatchInput =
                  DriverSearchBatchInput
                    { sendSearchRequestToDrivers = sendSearchRequestToDrivers',
                      merchant,
                      searchReq,
                      tripQuoteDetails = [tripQuoteDetail],
                      customerExtraFee = searchTry.customerExtraFee,
                      messageId = newBooking.id.getId,
                      isRepeatSearch
                    }
            result <- try @_ @SomeException (initiateDriverSearchBatch driverSearchBatchInput) --  remove CONSTRAINT quote_unique_reqid_bppid_quoteid UNIQUE (request_id, provider_id);, check searchTry key for scheduling
            case result of
              Right _ -> do
                if isValueAddNP
                  then BP.sendQuoteRepetitionUpdateToBAP booking ride newBooking.id bookingCReason.source driver vehicle
                  else cancelRideTransactionForNonReallocation (Just newBooking) booking Nothing merchant bookingCReason.source
              Left _ -> cancelRideTransactionForNonReallocation (Just newBooking) booking Nothing merchant bookingCReason.source
          else cancelRideTransactionForNonReallocation Nothing booking Nothing merchant bookingCReason.source
      _ -> cancelRideTransactionForNonReallocation Nothing booking Nothing merchant bookingCReason.source
  where
    cancelRideTransactionForNonReallocation :: Maybe SRB.Booking -> SRB.Booking -> Maybe Text -> DMerc.Merchant -> SBCR.CancellationSource -> Flow ()
    cancelRideTransactionForNonReallocation mbNewBooking booking mbEstimateId merchant cancellationSource = do
      Redis.del $ multipleRouteKey booking.transactionId
      Redis.del $ searchRequestKey booking.transactionId
      whenJust mbEstimateId $ \estimateId ->
        void $ clearCachedFarePolicyByEstOrQuoteId estimateId
      whenJust mbNewBooking $ \newBooking -> do
        bookingCancellationReason <- buildBookingCancellationReason newBooking
        QBCR.upsert bookingCancellationReason
        QRB.updateStatus newBooking.id SRB.CANCELLED
      void $ clearCachedFarePolicyByEstOrQuoteId booking.quoteId -- shouldn't be required for new booking
      BP.sendBookingCancelledUpdateToBAP booking merchant cancellationSource

    checkIfRepeatSearch searchTry booking driverArrivalTime isReallocationEnabled now = do
      transporterConfig <- QTC.findByMerchantOpCityId booking.merchantOperatingCityId (Just booking.transactionId) (Just "transactionId") >>= fromMaybeM (TransporterConfigNotFound booking.merchantOperatingCityId.getId)
      let searchRepeatLimit = transporterConfig.searchRepeatLimit
          isSearchTryValid = searchTry.validTill > now
          arrivedPickupThreshold = highPrecMetersToMeters transporterConfig.arrivedPickupThreshold
          driverHasNotArrived = isNothing driverArrivalTime || maybe True (> arrivedPickupThreshold) bookingCReason.driverDistToPickup
      return $
        searchTry.searchRepeatCounter < searchRepeatLimit
          && bookingCReason.source == SBCR.ByDriver
          && isSearchTryValid
          && fromMaybe False isReallocationEnabled
          && driverHasNotArrived

    buildBookingCancellationReason newBooking = do
      return $
        SBCR.BookingCancellationReason
          { bookingId = newBooking.id,
            rideId = Nothing,
            merchantId = Just newBooking.providerId,
            source = SBCR.ByApplication,
            reasonCode = Nothing,
            driverId = Nothing,
            additionalInfo = Just "Reallocation Failed",
            driverCancellationLocation = Nothing,
            driverDistToPickup = Nothing,
            ..
          }

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
  m ()
cancelRideTransaction booking ride bookingCReason merchantId rideEndedBy = do
  let driverId = cast ride.driverId
  void $ CQDGR.setDriverGoHomeIsOnRideStatus ride.driverId booking.merchantOperatingCityId False
  QDI.updateOnRide False (cast ride.driverId)
  void $ LF.rideDetails ride.id DRide.CANCELLED merchantId ride.driverId booking.fromLocation.lat booking.fromLocation.lon
  void $ QRide.updateStatus ride.id DRide.CANCELLED
  void $ QRide.updateRideEndedBy ride.id rideEndedBy
  QBCR.upsert bookingCReason
  void $ QRB.updateStatus booking.id SRB.CANCELLED
  when (bookingCReason.source == SBCR.ByDriver) $ QDriverStats.updateIdleTime driverId
