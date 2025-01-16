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
    CancelRideReq (..),
    CancelSearchReq (..),
    validateCancelRequest,
    validateCancelSearchRequest,
    cancelSearch,
    customerCancellationChargesCalculation,
    getCancellationCharges,
  )
where

import Data.Maybe (listToMaybe)
import Domain.Action.UI.Ride.CancelRide (driverDistanceToPickup)
import qualified Domain.Action.UI.Ride.CancelRide.Internal as CInternal
import qualified Domain.Types.Booking as SRB
import qualified Domain.Types.BookingCancellationReason as DBCR
import qualified Domain.Types.CancellationReason as DTCR
import Domain.Types.DriverLocation
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Ride as SRide
import qualified Domain.Types.SearchRequestForDriver as Domain
import qualified Domain.Types.SearchTry as ST
import Environment
import EulerHS.Prelude
import Kernel.External.Maps
import Kernel.Prelude (roundToIntegral)
import qualified Kernel.Storage.Esqueleto as Esq
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.Common
import Kernel.Utils.Servant.SignatureAuth (SignatureAuthResult (..))
import qualified Lib.DriverCoins.Coins as DC
import qualified Lib.DriverCoins.Types as DCT
import qualified SharedLogic.BehaviourManagement.CancellationRate as SCR
import SharedLogic.Booking
import SharedLogic.Cancel
import qualified SharedLogic.DriverPool as DP
import qualified SharedLogic.External.LocationTrackingService.Flow as LF
import SharedLogic.FareCalculator as FareCalculator
import SharedLogic.FarePolicy as SFP
import SharedLogic.Ride
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
import Tools.Constants
import Tools.Error
import Tools.Event
import qualified Tools.Notifications as Notify
import Utils.Common.Cac.KeyNameConstants

data CancelReq = CancelSearch CancelSearchReq | CancelRide CancelRideReq
  deriving (Show)

data CancelRideReq = CancelRideReq
  { bookingId :: Id SRB.Booking,
    cancelStatus :: Maybe Text,
    userReallocationEnabled :: Maybe Bool,
    cancellationReason :: Maybe Text
  }
  deriving (Show)

newtype CancelSearchReq = CancelSearchReq
  { transactionId :: Text
  }
  deriving (Show)

cancel ::
  CancelRideReq ->
  DM.Merchant ->
  SRB.Booking ->
  Maybe ST.SearchTry ->
  Flow (Bool, Maybe PriceAPIEntity)
cancel req merchant booking mbActiveSearchTry = do
  CS.whenBookingCancellable booking.id $ do
    mbRide <- QRide.findActiveByRBId req.bookingId
    transporterConfig <- CCT.findByMerchantOpCityId booking.merchantOperatingCityId (Just (TransactionId (Id booking.transactionId))) >>= fromMaybeM (TransporterConfigNotFound booking.merchantOperatingCityId.getId)
    whenJust mbRide $ \ride -> do
      void $ CQDGR.setDriverGoHomeIsOnRideStatus ride.driverId booking.merchantOperatingCityId False
      updateOnRideStatusWithAdvancedRideCheck ride.driverId mbRide
      Redis.unlockRedis (offerQuoteLockKeyWithCoolDown ride.driverId)
      void $ LF.rideDetails ride.id SRide.CANCELLED merchant.id ride.driverId booking.fromLocation.lat booking.fromLocation.lon Nothing Nothing
      QRide.updateStatus ride.id SRide.CANCELLED
      when (booking.isScheduled) $ QDI.updateLatestScheduledBookingAndPickup Nothing Nothing ride.driverId

    (disToPickup, mbLocation) <- getDistanceToPickup booking mbRide
    let currentLocation = getCoordinates <$> mbLocation
    bookingCR <- buildBookingCancellationReason disToPickup currentLocation mbRide
    QBCR.upsert bookingCR
    QRB.updateStatus booking.id SRB.CANCELLED
    when booking.isScheduled $ removeBookingFromRedis booking
    fork "DriverRideCancelledCoin" $ do
      whenJust mbRide $ \ride -> do
        logDebug $ "RideCancelled Coin Event by customer distance to pickup" <> show disToPickup
        logDebug "RideCancelled Coin Event by customer"
        DC.driverCoinsEvent ride.driverId merchant.id booking.merchantOperatingCityId (DCT.Cancellation ride.createdAt booking.distanceToPickup disToPickup) (Just $ ride.id.getId) ride.vehicleVariant

        whenJust booking.riderId (DP.addDriverToRiderCancelledList ride.driverId)

    whenJust mbRide $ \ride -> do
      triggerRideCancelledEvent RideEventData {ride = ride{status = SRide.CANCELLED}, personId = ride.driverId, merchantId = merchant.id}
      triggerBookingCancelledEvent BookingEventData {booking = booking{status = SRB.CANCELLED}, personId = ride.driverId, merchantId = merchant.id}
      fork "incrementCancelledCount based on nammatag" $ do
        rideTags <- CInternal.updateNammaTagsForCancelledRide booking ride bookingCR
        when (validDriverCancellation `elem` rideTags) $ do
          let windowSize = toInteger $ fromMaybe 7 transporterConfig.cancellationRateWindow
          void $ SCR.incrementCancelledCount ride.driverId windowSize

    logTagInfo ("bookingId-" <> getId req.bookingId) ("Cancellation reason " <> show bookingCR.source)

    cancellationCharge <- do
      case mbRide of
        Just ride -> do
          case (transporterConfig.canAddCancellationFee, ride.cancellationFeeIfCancelled) of
            (False, _) -> return Nothing
            (True, Just cancellationCharges) -> return $ Just PriceAPIEntity {amount = cancellationCharges, currency = booking.currency}
            (True, Nothing) -> customerCancellationChargesCalculation booking (Just ride) disToPickup
        _ -> return Nothing
    case (cancellationCharge, booking.riderId) of
      (Just fee, Just riderId) -> do
        riderDetails <- QRD.findById riderId
        whenJust riderDetails $ \riderD -> do
          QRD.updateCancellationDues (fee.amount + riderD.cancellationDues) riderId
      _ -> logInfo "RiderId or Cancellation charge not found"

    isReallocated <-
      case mbRide of
        Just ride -> do
          driver <- QPers.findById ride.driverId >>= fromMaybeM (PersonNotFound ride.driverId.getId)
          fork "cancelRide - Notify driver" $
            Notify.notifyOnCancel booking.merchantOperatingCityId booking driver bookingCR.source
          isValueAddNP <- CQVAN.isValueAddNP booking.bapId
          vehicle <- QVeh.findById ride.driverId >>= fromMaybeM (DriverWithoutVehicle ride.driverId.getId)
          reAllocateBookingIfPossible isValueAddNP (fromMaybe False req.userReallocationEnabled) merchant booking ride driver vehicle bookingCR False
        Nothing -> return False
    whenJust mbActiveSearchTry $ cancelSearch merchant.id

    return (isReallocated, cancellationCharge)
  where
    buildBookingCancellationReason disToPickup currentLocation mbRide = do
      return $
        DBCR.BookingCancellationReason
          { bookingId = req.bookingId,
            rideId = (.id) <$> mbRide,
            merchantId = Just booking.providerId,
            source = DBCR.ByUser,
            reasonCode = DTCR.CancellationReasonCode <$> req.cancellationReason,
            driverId = (.driverId) <$> mbRide,
            additionalInfo = Nothing,
            driverCancellationLocation = currentLocation,
            driverDistToPickup = disToPickup,
            distanceUnit = booking.distanceUnit,
            merchantOperatingCityId = Just booking.merchantOperatingCityId,
            ..
          }

getDistanceToPickup :: SRB.Booking -> Maybe SRide.Ride -> Flow (Maybe Meters, Maybe DriverLocation)
getDistanceToPickup booking mbRide = do
  case mbRide of
    Just ride -> do
      mbLocation <- do
        driverLocations <- try @_ @SomeException $ LF.driversLocation [ride.driverId]
        case driverLocations of
          Left err -> do
            logError ("Failed to fetch Driver Location with error : " <> show err)
            return Nothing
          Right locations -> return $ listToMaybe locations
      case mbLocation of
        Just location -> do
          distance <- driverDistanceToPickup booking (getCoordinates location) (getCoordinates booking.fromLocation)
          return (Just distance, Just location)
        Nothing -> return (Nothing, Nothing)
    _ -> return (Nothing, Nothing)

getCancellationCharges :: SRB.Booking -> Flow (Maybe PriceAPIEntity)
getCancellationCharges booking = do
  transporterConfig <- CCT.findByMerchantOpCityId booking.merchantOperatingCityId (Just (TransactionId (Id booking.transactionId))) >>= fromMaybeM (TransporterConfigNotFound booking.merchantOperatingCityId.getId)
  if transporterConfig.canAddCancellationFee
    then do
      mbRide <- QRide.findActiveByRBId booking.id
      (distanceToPickup, _) <- getDistanceToPickup booking mbRide
      customerCancellationChargesCalculation booking mbRide distanceToPickup
    else return Nothing

customerCancellationChargesCalculation :: SRB.Booking -> Maybe SRide.Ride -> Maybe Meters -> Flow (Maybe PriceAPIEntity)
customerCancellationChargesCalculation booking mbRide currDistanceToPickup = do
  mbFarePolicy <- SFP.getFarePolicyByEstOrQuoteIdWithoutFallback booking.quoteId
  let mbCancellationAndNoShowConfigs = (.cancellationFarePolicy) =<< mbFarePolicy
  case (mbRide, mbCancellationAndNoShowConfigs) of
    (Just ride, Just cancellationAndNoShowConfigs) -> do
      driverDetails <- QDI.findById ride.driverId >>= fromMaybeM (PersonNotFound ride.driverId.getId)
      timeSpentByDriver <- getTimeSpentByDriver ride
      initialDistanceToPickup <- getInitialDistanceToPickup ride
      let isTimeSpentByDriverLessThanThreshold = timeSpentByDriver <= (cancellationAndNoShowConfigs.freeCancellationTimeSeconds.getSeconds)
      if isTimeSpentByDriverLessThanThreshold || driverDetails.hasAdvanceBooking
        then return Nothing
        else do
          logDebug $ "Params passed to calculateCancellationCharges: cancellationAndNoShowConfigs " <> show cancellationAndNoShowConfigs <> "| initialDistanceToPickup: " <> show initialDistanceToPickup <> "| currDistanceToPickup: " <> show currDistanceToPickup <> "| timeSpentByDriver: " <> show timeSpentByDriver <> "| estimatedFare: " <> show booking.estimatedFare
          let cancellationCharges = FareCalculator.calculateCancellationCharges cancellationAndNoShowConfigs initialDistanceToPickup currDistanceToPickup timeSpentByDriver booking.estimatedFare
          QRide.updateCancellationFeeIfCancelledField (Just cancellationCharges) ride.id
          return $ Just $ PriceAPIEntity {amount = cancellationCharges, currency = booking.currency}
    _ -> do
      logError $ "customerCancellationChargesCalculation: Ride or cancellation and now show configs not found for fare policy" <> show mbFarePolicy
      return Nothing
  where
    getInitialDistanceToPickup ride = do
      if ride.isAdvanceBooking
        then do
          forM ride.previousRideTripEndPos $ \location -> do
            driverDistanceToPickup booking (getCoordinates location) (getCoordinates booking.fromLocation)
        else return booking.distanceToPickup
    getTimeSpentByDriver ride = do
      now <- getCurrentTime
      if ride.isAdvanceBooking
        then do
          case ride.previousRideTripEndTime of
            Just endTime -> return $ roundToIntegral $ diffUTCTime now endTime
            Nothing -> return $ roundToIntegral $ diffUTCTime now ride.createdAt
        else return $ roundToIntegral $ diffUTCTime now ride.createdAt

cancelSearch ::
  ( CacheFlow m r,
    EsqDBFlow m r,
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
    Notify.notifyOnCancelSearchRequest searchTry.merchantOperatingCityId driver_ driverReq.searchTryId searchTry.tripCategory

validateCancelSearchRequest ::
  ( CacheFlow m r,
    EsqDBFlow m r
  ) =>
  Id DM.Merchant ->
  SignatureAuthResult ->
  CancelSearchReq ->
  m ST.SearchTry
validateCancelSearchRequest merchantId _ req = do
  let transactionId = req.transactionId
  searchReq <- QSR.findByTransactionIdAndMerchantId transactionId merchantId >>= fromMaybeM (SearchRequestNotFound $ "transactionId-" <> transactionId <> ",merchantId-" <> merchantId.getId)
  QST.findTryByRequestId searchReq.id >>= fromMaybeM (SearchTryDoesNotExist $ "searchRequestId-" <> searchReq.id.getId)

validateCancelRequest ::
  ( EsqDBFlow m r,
    CacheFlow m r
  ) =>
  Id DM.Merchant ->
  SignatureAuthResult ->
  CancelRideReq ->
  m (DM.Merchant, SRB.Booking)
validateCancelRequest merchantId _ req = do
  merchant <-
    QM.findById merchantId
      >>= fromMaybeM (MerchantNotFound merchantId.getId)
  booking <- QRB.findById req.bookingId >>= fromMaybeM (BookingDoesNotExist req.bookingId.getId)
  let merchantId' = booking.providerId
  unless (merchantId' == merchantId) $ throwError AccessDenied
  return (merchant, booking)
