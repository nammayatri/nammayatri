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
import qualified Domain.Types.Merchant.TransporterConfig as DTC
import qualified Domain.Types.Ride as SRide
import qualified Domain.Types.SearchRequestForDriver as Domain
import qualified Domain.Types.SearchTry as ST
import Environment
import EulerHS.Prelude
import Kernel.External.Maps
import Kernel.Prelude (roundToIntegral)
import qualified Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.Common
import Kernel.Utils.Servant.SignatureAuth (SignatureAuthResult (..))
import qualified Lib.DriverCoins.Coins as DC
import qualified Lib.DriverCoins.Types as DCT
import SharedLogic.Cancel
import qualified SharedLogic.DriverPool as DP
import qualified SharedLogic.External.LocationTrackingService.Flow as LF
import qualified SharedLogic.SearchTryLocker as CS
import qualified Storage.Cac.TransporterConfig as CCT
import qualified Storage.CachedQueries.Driver.GoHomeRequest as CQDGR
import qualified Storage.CachedQueries.Merchant as QM
import qualified Storage.CachedQueries.ValueAddNP as CQVAN
import qualified Storage.Queries.Booking as QRB
import qualified Storage.Queries.BookingCancellationReason as QBCR
import qualified Storage.Queries.DriverInformation as QDI
import qualified Storage.Queries.DriverQuote as QDQ
import qualified Storage.Queries.Person as QPers
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.Ride as QRide
import qualified Storage.Queries.RiderDetails as QRD
import qualified Storage.Queries.SearchRequest as QSR
import qualified Storage.Queries.SearchRequestForDriver as QSRD
import qualified Storage.Queries.SearchTry as QST
import qualified Storage.Queries.Vehicle as QVeh
import Tools.Error
import Tools.Event
import qualified Tools.Notifications as Notify
import Utils.Common.Cac.KeyNameConstants

data CancelReq = CancelReq
  { bookingId :: Id SRB.Booking,
    cancelStatus :: Maybe Text,
    userReallocationEnabled :: Maybe Bool
  }
  deriving (Show)

newtype CancelSearchReq = CancelSearchReq
  { transactionId :: Text
  }
  deriving (Show)

cancel ::
  CancelReq ->
  DM.Merchant ->
  SRB.Booking ->
  Maybe ST.SearchTry ->
  Flow Bool
cancel req merchant booking mbActiveSearchTry = do
  CS.whenBookingCancellable booking.id $ do
    mbRide <- QRide.findActiveByRBId req.bookingId
    whenJust mbRide $ \ride -> do
      void $ CQDGR.setDriverGoHomeIsOnRideStatus ride.driverId booking.merchantOperatingCityId False
      QDI.updateOnRide False (cast ride.driverId)
      void $ LF.rideDetails ride.id SRide.CANCELLED merchant.id ride.driverId booking.fromLocation.lat booking.fromLocation.lon
      QRide.updateStatus ride.id SRide.CANCELLED

    bookingCR <- buildBookingCancellationReason
    QBCR.upsert bookingCR
    QRB.updateStatus booking.id SRB.CANCELLED

    fork "DriverRideCancelledCoin and CustomerCancellationDuesCalculation Location trakking" $ do
      whenJust mbRide $ \ride -> do
        mbLocation <- do
          driverLocations <- try @_ @SomeException $ LF.driversLocation [ride.driverId]
          case driverLocations of
            Left err -> do
              logError ("Failed to fetch Driver Location with error : " <> show err)
              return Nothing
            Right locations -> return $ listToMaybe locations
        disToPickup <- forM mbLocation $ \location -> do
          driverDistanceToPickup booking.providerId booking.merchantOperatingCityId (getCoordinates location) (getCoordinates booking.fromLocation)
        logDebug $ "RideCancelled Coin Event by customer distance to pickup" <> show disToPickup
        logDebug "RideCancelled Coin Event by customer"
        DC.driverCoinsEvent ride.driverId merchant.id booking.merchantOperatingCityId (DCT.Cancellation ride.createdAt booking.distanceToPickup disToPickup)
        transporterConfig <- CCT.findByMerchantOpCityId booking.merchantOperatingCityId (Just (TransactionId (Id booking.transactionId))) >>= fromMaybeM (TransporterConfigNotFound booking.merchantOperatingCityId.getId)

        whenJust booking.riderId (DP.addDriverToRiderCancelledList ride.driverId)
        when transporterConfig.canAddCancellationFee do
          customerCancellationChargesCalculation transporterConfig booking disToPickup

    whenJust mbRide $ \ride -> do
      triggerRideCancelledEvent RideEventData {ride = ride{status = SRide.CANCELLED}, personId = ride.driverId, merchantId = merchant.id}
      triggerBookingCancelledEvent BookingEventData {booking = booking{status = SRB.CANCELLED}, personId = ride.driverId, merchantId = merchant.id}

    logTagInfo ("bookingId-" <> getId req.bookingId) ("Cancellation reason " <> show bookingCR.source)

    isReallocated <-
      case mbRide of
        Just ride -> do
          driver <- QPers.findById ride.driverId >>= fromMaybeM (PersonNotFound ride.driverId.getId)
          fork "cancelRide - Notify driver" $
            Notify.notifyOnCancel booking.merchantOperatingCityId booking driver bookingCR.source
          isValueAddNP <- CQVAN.isValueAddNP booking.bapId
          vehicle <- QVeh.findById ride.driverId >>= fromMaybeM (DriverWithoutVehicle ride.driverId.getId)
          reAllocateBookingIfPossible isValueAddNP (fromMaybe False req.userReallocationEnabled) merchant booking ride driver vehicle bookingCR
        Nothing -> return False
    whenJust mbActiveSearchTry $ cancelSearch merchant.id
    return isReallocated
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

customerCancellationChargesCalculation :: KvDbFlow m r => DTC.TransporterConfig -> SRB.Booking -> Maybe Meters -> m ()
customerCancellationChargesCalculation transporterConfig booking disToPickup = do
  logInfo $ "Entered CustomerCancellationDuesCalculation: " <> show disToPickup
  whenJust disToPickup $ \driverDistToPickup -> do
    now <- getCurrentTime
    let driverBookingDuration = roundToIntegral $ diffUTCTime now booking.createdAt
    let condition = (driverBookingDuration > transporterConfig.driverTimeSpentOnPickupThresholdOnCancel) && (driverDistToPickup < transporterConfig.driverDistanceToPickupThresholdOnCancel)
    logInfo $ "Verifying condition1 for cancellation: " <> show condition <> " " <> show driverBookingDuration
    isChargable <-
      if not condition
        then do
          case booking.distanceToPickup of
            Just acutalDistanceToPickup ->
              return $ (acutalDistanceToPickup - driverDistToPickup) > transporterConfig.driverDistanceTravelledOnPickupThresholdOnCancel
            Nothing -> return condition
        else return condition
    when isChargable $ do
      riderId <- booking.riderId & fromMaybeM (RiderDetailsDoNotExist "BOOKING" booking.id.getId)
      rider <- QRD.findById riderId >>= fromMaybeM (RiderDetailsNotFound riderId.getId)
      QRD.updateCancellationDues riderId (rider.cancellationDues + transporterConfig.cancellationFee)

cancelSearch ::
  ( KvDbFlow m r,
    Esq.EsqDBReplicaFlow m r
  ) =>
  Id DM.Merchant ->
  ST.SearchTry ->
  m ()
cancelSearch _merchantId searchTry = do
  driverSearchReqs <- QSRD.findAllActiveBySRId searchTry.requestId Domain.Active
  QST.cancelActiveTriesByRequestId searchTry.requestId
  QSRD.setInactiveAndPulledByIds $ (.id) <$> driverSearchReqs
  QDQ.setInactiveBySRId searchTry.requestId
  for_ driverSearchReqs $ \driverReq -> do
    driver_ <- QPerson.findById driverReq.driverId >>= fromMaybeM (PersonNotFound driverReq.driverId.getId)
    Notify.notifyOnCancelSearchRequest searchTry.merchantOperatingCityId driver_ driverReq.searchTryId

validateCancelSearchRequest ::
  KvDbFlow m r =>
  Id DM.Merchant ->
  SignatureAuthResult ->
  CancelSearchReq ->
  m ST.SearchTry
validateCancelSearchRequest merchantId _ req = do
  let transactionId = req.transactionId
  searchReq <- QSR.findByTransactionIdAndMerchantId transactionId merchantId >>= fromMaybeM (SearchRequestNotFound $ "transactionId-" <> transactionId <> ",merchantId-" <> merchantId.getId)
  QST.findTryByRequestId searchReq.id >>= fromMaybeM (SearchTryDoesNotExist $ "searchRequestId-" <> searchReq.id.getId)

validateCancelRequest ::
  ( KvDbFlow m r
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
