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
    getCancellationCharges,
  )
where

import Data.Maybe
import qualified Data.Text as Text
import Domain.Action.UI.Ride.CancelRide.Internal
import qualified Domain.Types.Booking as SRB
import qualified Domain.Types.BookingCancellationReason as DBCR
import qualified Domain.Types.CancellationReason as DTCR
import qualified Domain.Types.Common as DTC
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Ride as SRide
import qualified Domain.Types.SearchRequestForDriver as Domain
import qualified Domain.Types.SearchTry as ST
import Environment
import EulerHS.Prelude
import Kernel.Beam.Functions
import Kernel.External.Maps
import Kernel.External.Types (ServiceFlow)
import qualified Kernel.Storage.Clickhouse.Config as CH
import qualified Kernel.Storage.Esqueleto as Esq
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.Common
import Kernel.Utils.Servant.SignatureAuth (SignatureAuthResult (..))
import Lib.ConfigPilot.Interface.Types (getOneConfig)
import qualified Lib.DriverCoins.Coins as DC
import qualified Lib.DriverCoins.Types as DCT
import qualified Lib.Yudhishthira.Types as LYT
import qualified SharedLogic.Analytics as Analytics
import qualified SharedLogic.BehaviourManagement.CancellationRate as SCR
import qualified SharedLogic.BehaviourManagement.PickupStall as PickupStall
import SharedLogic.Booking
import SharedLogic.Cancel
import qualified SharedLogic.CancellationDues as SCD
import qualified SharedLogic.CancellationFault as CancellationFault
import qualified SharedLogic.DriverPool as DP
import qualified SharedLogic.External.LocationTrackingService.Flow as LF
import qualified SharedLogic.External.LocationTrackingService.Types as LT
import qualified SharedLogic.MetricsLabels as SML
import SharedLogic.Ride
import qualified SharedLogic.SearchTryLocker as CS
import qualified Storage.CachedQueries.Driver.GoHomeRequest as CQDGR
import qualified Storage.CachedQueries.Merchant as QM
import qualified Storage.CachedQueries.ValueAddNP as CQVAN
import Storage.ConfigPilot.Config.TransporterConfig (TransporterConfigDimensions (..))
import qualified Storage.Queries.Booking as QRB
import qualified Storage.Queries.BookingCancellationReason as QBCR
import qualified Storage.Queries.CancellationDuesDetails as QCDD
import qualified Storage.Queries.DriverInformation as QDI
import qualified Storage.Queries.DriverQuote as QDQ
import qualified Storage.Queries.Person as QPers
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.QueriesExtra.SearchRequestLite as QSRLite
import qualified Storage.Queries.Ride as QRide
import qualified Storage.Queries.RiderDetails as QRD
import qualified Storage.Queries.SearchRequestForDriver as QSRD
import qualified Storage.Queries.SearchTry as QST
import qualified Storage.Queries.Vehicle as QVeh
import Tools.Constants
import Tools.Error
import Tools.Event
import qualified Tools.Metrics as Metrics
import qualified Tools.Notifications as Notify

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
  Flow (Bool, Maybe PriceAPIEntity, Maybe SRide.Ride)
cancel req merchant booking mbActiveSearchTry = do
  CS.whenBookingCancellable booking.id $ do
    mbRide <- QRide.findActiveByRBId req.bookingId
    transporterConfig <- getOneConfig (TransporterConfigDimensions {merchantOperatingCityId = booking.merchantOperatingCityId.getId}) Nothing >>= fromMaybeM (TransporterConfigNotFound booking.merchantOperatingCityId.getId)
    let prepaidSubscriptionAndWalletEnabled = fromMaybe False merchant.prepaidSubscriptionAndWalletEnabled
    when prepaidSubscriptionAndWalletEnabled $ whenJust mbRide $ \ride -> releaseLien booking ride
    whenJust mbRide $ \ride -> do
      void $ CQDGR.setDriverGoHomeIsOnRideStatus ride.driverId booking.merchantOperatingCityId False
      updateOnRideStatusWithAdvancedRideCheck ride.driverId mbRide
      Redis.unlockRedis (offerQuoteLockKeyWithCoolDown ride.driverId)
      void $ LF.rideDetails ride.id SRide.CANCELLED merchant.id ride.driverId booking.fromLocation.lat booking.fromLocation.lon Nothing (Just $ (LT.Car $ LT.CarRideInfo {pickupLocation = LatLong (booking.fromLocation.lat) (booking.fromLocation.lon), minDistanceBetweenTwoPoints = Nothing, rideStops = Just $ map (\stop -> LatLong stop.lat stop.lon) booking.stops}))
      QRide.updateStatus ride.id SRide.CANCELLED
      when (booking.isScheduled) $ QDI.updateLatestScheduledBookingAndPickup Nothing Nothing ride.driverId

    (disToPickup, mbLocation) <- getDistanceToPickup booking mbRide
    let currentLocation = getCoordinates <$> mbLocation
    bookingCR <- buildBookingCancellationReason disToPickup currentLocation mbRide
    QBCR.upsert bookingCR
    cityLabel <- SML.getCityLabel booking.merchantOperatingCityId
    Metrics.incrementRideCancelledCount merchant.shortId.getShortId cityLabel (show booking.vehicleServiceTier) (show bookingCR.source) (SML.distanceBucketLabel booking.estimatedDistance)
    QRB.updateStatus booking.id SRB.CANCELLED
    when booking.isScheduled $ removeBookingFromRedis booking
    fork "DriverRideCancelledCoin" $ do
      whenJust mbRide $ \ride -> do
        logDebug $ "RideCancelled Coin Event by customer distance to pickup" <> show disToPickup
        logDebug "RideCancelled Coin Event by customer"
        DC.driverCoinsEvent ride.driverId Nothing merchant.id booking.merchantOperatingCityId (DCT.Cancellation ride.createdAt booking.distanceToPickup disToPickup DCT.CancellationByCustomer (fromMaybe (DTCR.CancellationReasonCode "Other") bookingCR.reasonCode)) (Just $ ride.id.getId) ride.vehicleVariant (Just booking.vehicleServiceTier) (Just booking.configInExperimentVersions) Nothing

        let riderBlacklistTtl = fromMaybe 3600 transporterConfig.driverRiderBlacklistDurationSeconds
        whenJust booking.riderId (DP.addDriverToRiderCancelledList riderBlacklistTtl ride.driverId)

    whenJust mbRide $ \ride -> do
      triggerRideCancelledEvent RideEventData {ride = ride{status = SRide.CANCELLED}, personId = ride.driverId, merchantId = merchant.id}
      triggerBookingCancelledEvent BookingEventData {booking = booking{status = SRB.CANCELLED}, personId = ride.driverId, merchantId = merchant.id}

    -- Driver-fault attribution: if the pickup progress monitor had an active stall case
    -- when the customer cancelled, count it against the driver (unless the monitor already
    -- recorded it at a terminal stage — the ride tag marks that). Runs regardless of
    -- whether this cancel results in reallocation: a customer forced to cancel/reallocate
    -- because the driver never moved is exactly the case this must catch.
    whenJust mbRide $ \ride ->
      fork "record pickup stall on customer cancel" $ do
        let alreadyRecorded = any (\(LYT.TagNameValue t) -> PickupStall.pickupStallRideTagPrefix `Text.isPrefixOf` t) (fromMaybe [] ride.rideTags)
        unless alreadyRecorded $ do
          mbMonitorState :: Maybe PickupStall.PickupProgressState <- Redis.safeGet (PickupStall.pickupProgressStateKey ride.id)
          whenJust (mbMonitorState >>= (.activeCase)) $ \stallCase -> do
            QRide.updateRideTags (Just $ PickupStall.mkPickupStallRideTag stallCase : fromMaybe [] ride.rideTags) ride.id
            PickupStall.recordPickupStall transporterConfig ride.driverId ride.merchantOperatingCityId ride.id stallCase PickupStall.CustomerCancelledDriverAtFault

    isReallocated <-
      case mbRide of
        Just ride -> do
          driver <- QPers.findById ride.driverId >>= fromMaybeM (PersonNotFound ride.driverId.getId)
          fork "cancelRide - Notify driver" $
            Notify.notifyOnCancel booking.merchantOperatingCityId ride.id booking driver bookingCR.source
          isValueAddNP <- CQVAN.isValueAddNP booking.bapId
          vehicle <- QVeh.findById ride.driverId >>= fromMaybeM (DriverWithoutVehicle ride.driverId.getId)
          isReallocat <- reAllocateBookingIfPossible isValueAddNP (fromMaybe False req.userReallocationEnabled) merchant booking ride driver vehicle bookingCR False
          -- newRide <- QRide.findById ride.id >>= fromMaybeM (RideDoesNotExist ride.id.getId)
          -- let cancellationFeeIfCancelled = maybe Nothing (\charges-> Just PriceAPIEntity {amount = charges, currency = booking.currency}) newRide.cancellationFeeIfCancelled
          return isReallocat
        Nothing -> return False

    if isReallocated
      then do
        return (isReallocated, Nothing, Nothing)
      else do
        cancellationCharges <- withTryCatch "cancellationCharges" $ do
          case mbRide of
            Just ride -> do
              (signals, mbFaultVerdict) <- buildCancellationContext booking ride transporterConfig DCT.CancellationByCustomer bookingCR.reasonCode disToPickup
              rideTags <- updateNammaTagsForCancelledRide booking ride bookingCR transporterConfig mbFaultVerdict
              when (validDriverCancellation `elem` rideTags) $ do
                let windowSize = toInteger $ fromMaybe 7 transporterConfig.cancellationRateWindow
                void $ SCR.incrementCancelledCount ride.driverId windowSize
              case booking.riderId of
                Just riderId -> do
                  riderDetails <- QRD.findById riderId >>= fromMaybeM (RiderDetailsNotFound riderId.getId)
                  void $ QRD.updateCancelledRidesCount riderId.getId
                  -- Charge eligibility: the fault verdict decides when CANCELLATION_FAULT_VERDICT
                  -- rules are configured; otherwise fall back to the legacy tag gate so
                  -- cities migrate to verdict gating one at a time.
                  when (transporterConfig.canAddCancellationFee && isNothing mbFaultVerdict) $
                    logWarning $ "No CANCELLATION_FAULT_VERDICT rules configured for city " <> booking.merchantOperatingCityId.getId <> " — falling back to tag-based charge gating, rideId: " <> ride.id.getId
                  if CancellationFault.customerAtFaultOrLegacy (validCustomerCancellation `elem` rideTags) mbFaultVerdict
                    then do
                      QRD.updateValidCancellationsCount riderId.getId
                      mbExistingCancellationDuesDetails <- QCDD.findByRideId ride.id
                      chargesOutcome <- case ride.cancellationFeeIfCancelled of
                        Just cancelCharges ->
                          return
                            CancellationChargesOutcome
                              { fee = Just cancelCharges,
                                tax = mbExistingCancellationDuesDetails >>= (.cancellationFeeTax),
                                overdueFee = mbExistingCancellationDuesDetails >>= (.overdueCancellationCharge),
                                overdueTax = mbExistingCancellationDuesDetails >>= (.overdueCancellationTax),
                                commission = mbExistingCancellationDuesDetails >>= (.cancellationCommission),
                                overdueCommission = mbExistingCancellationDuesDetails >>= (.overdueCancellationCommission)
                              }
                        Nothing -> do
                          (mbOutcome, _mbLogicVersion) <- customerCancellationChargesCalculation booking ride riderDetails DCT.CancellationByCustomer bookingCR.reasonCode ride.cancellationChargesLogicVersion signals mbFaultVerdict
                          case mbOutcome of
                            Just o -> do
                              logTagInfo ("bookingId-" <> getId req.bookingId) ("cancellation dues: " <> show o.fee <> " tax: " <> show o.tax)
                              return o
                            Nothing -> return (CancellationChargesOutcome Nothing Nothing Nothing Nothing Nothing Nothing)
                      let totalCharges = fromMaybe 0 chargesOutcome.fee + fromMaybe 0 chargesOutcome.tax
                      SCD.applyCancellationCharge
                        SCD.ApplyCancellationChargeReq
                          { ride = ride,
                            riderId = riderId,
                            currentDues = riderDetails.cancellationDues,
                            totalCharges = totalCharges,
                            currency = booking.currency,
                            cancellationFee = chargesOutcome.fee,
                            cancellationFeeTax = chargesOutcome.tax,
                            overdueCancellationCharge = chargesOutcome.overdueFee,
                            overdueCancellationTax = chargesOutcome.overdueTax,
                            cancellationCommission = chargesOutcome.commission,
                            overdueCancellationCommission = chargesOutcome.overdueCommission
                          }
                      when (totalCharges > 0) $
                        QRD.updateCancellationDueRidesCount riderId.getId
                      return (chargesOutcome.fee, chargesOutcome.tax, chargesOutcome.overdueFee, chargesOutcome.overdueTax)
                    else return (Nothing, Nothing, Nothing, Nothing)
                Nothing -> return (Nothing, Nothing, Nothing, Nothing)
            Nothing -> return (Nothing, Nothing, Nothing, Nothing)
        logTagInfo ("bookingId-" <> getId req.bookingId) ("Cancellation charges: " <> show cancellationCharges)
        (cancelChargesBase, cancelTax) <- case cancellationCharges of
          Left e -> do
            logError $ "Error in getting cancellation charges - " <> show e
            return (Nothing, Nothing)
          Right (charges, tax, _overdueCharge, _overdueTax) -> do
            let totalAmount = case charges of
                  Just c -> Just (c + fromMaybe 0 tax)
                  Nothing -> Nothing
            void $ case mbRide of
              Just ride -> do
                logTagInfo ("bookingId-" <> getId req.bookingId) ("cancellation charges onCancel: " <> show totalAmount <> " base: " <> show charges <> " tax: " <> show tax)
                QRide.updateCancellationChargesOnCancel totalAmount ride.cancellationChargesLogicVersion ride.id
              Nothing -> return ()
            return (charges, tax)
        let cancellationTaxAmount = fromMaybe 0 cancelTax
            -- base + tax kept separate; total built only here for the on_cancel CancellationTerm
            cancelCharges = (\base -> PriceAPIEntity {amount = base + cancellationTaxAmount, currency = booking.currency}) <$> cancelChargesBase

        logTagInfo ("bookingId-" <> getId req.bookingId) ("cancellationCharges: " <> show cancelCharges)
        logTagInfo ("bookingId-" <> getId req.bookingId) ("Cancellation reason " <> show bookingCR.source)

        -- BPP-side: create finance ledger entries from base + tax (no add-then-subtract)
        whenJust cancelChargesBase $ \baseCancellation -> do
          whenJust mbRide $ \ride -> do
            let isPrepaidSubscriptionAndWalletEnabled = fromMaybe False merchant.prepaidSubscriptionAndWalletEnabled
            when ((isPrepaidSubscriptionAndWalletEnabled || transporterConfig.driverWalletConfig.enableDriverWallet) && baseCancellation + cancellationTaxAmount > 0) $
              createCancellationLedgerEntries booking ride baseCancellation cancellationTaxAmount transporterConfig

        whenJust mbActiveSearchTry $ cancelSearch merchant.id
        -- Reload ride by primary key to pick up persisted cancellationChargesOnCancel
        updatedRide <- case mbRide of
          Just ride -> QRide.findById ride.id
          Nothing -> pure Nothing
        return (isReallocated, cancelCharges, updatedRide)
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

-- Cancel Search is only allowed to be called before Init happens on Driver App (i.e, when booking is created)
cancelSearch ::
  ( CacheFlow m r,
    EsqDBFlow m r,
    ServiceFlow m r,
    HasFlowEnv m r '["maxNotificationShards" ::: Int],
    Redis.HedisLTSFlowEnv r,
    Esq.EsqDBReplicaFlow m r,
    HasField "serviceClickhouseCfg" r CH.ClickhouseCfg,
    HasField "serviceClickhouseEnv" r CH.ClickhouseEnv
  ) =>
  Id DM.Merchant ->
  ST.SearchTry ->
  m ()
cancelSearch merchantId searchTry = do
  searchRequest <- QSRLite.findByIdLite searchTry.requestId >>= fromMaybeM (SearchRequestNotFound searchTry.requestId.getId)
  callWithErrorHandling searchRequest.transactionId $ do
    -- Lock Description: This is a Lock held between Init and Cancel Search, if Init is OnGoing the Booking will be created post the lock release and Cancel Search will fail with `CancelSearchLockNotAcquired`.
    -- Lock Release: Any Exceptions or at end of this function.
    cancelSearchInitLockAcquired <- Redis.tryLockRedis (mkCancelSearchInitLockKey searchRequest.transactionId) 30
    logError $ "cancelSearchInitLock | cancelSearch acquire | txn=" <> searchRequest.transactionId <> " acquired=" <> show cancelSearchInitLockAcquired
    unless cancelSearchInitLockAcquired $
      throwError CancelSearchLockNotAcquired
    when (DTC.isDynamicOfferTrip searchTry.tripCategory) $ do
      mbActiveBooking <- runInMasterDbAndRedis $ QRB.findByTransactionIdAndStatuses searchRequest.transactionId [SRB.NEW, SRB.TRIP_ASSIGNED]
      whenJust mbActiveBooking $ \_ ->
        throwError RideRequestAlreadyAccepted
    driverSearchReqs <- QSRD.findAllActiveBySRId searchTry.requestId Domain.Active
    QST.cancelActiveTriesByRequestId searchTry.requestId
    QSRD.setInactiveAndPulledByIds driverSearchReqs
    QDQ.setInactiveBySRId searchTry.requestId
    mbTransporterConfig <- getOneConfig (TransporterConfigDimensions {merchantOperatingCityId = searchTry.merchantOperatingCityId.getId}) Nothing
    for_ driverSearchReqs $ \driverReq -> do
      -- free the driver's parallel-request slot; it otherwise stays consumed
      -- against maxParallelSearchRequests until the entry's validTill passes
      DP.removeSearchReqIdFromMap merchantId driverReq.driverId driverReq.requestId
      DP.decrementSrdSentCount driverReq.createdAt driverReq.driverId
      whenJust mbTransporterConfig $ \transporterConfig ->
        when transporterConfig.analyticsConfig.enableFleetOperatorDashboardAnalytics $
          Analytics.updateOperatorAnalyticsAcceptationTotalRequestAndPassedCount driverReq.driverId transporterConfig False False False True
      driver_ <- QPerson.findById driverReq.driverId >>= fromMaybeM (PersonNotFound driverReq.driverId.getId)
      Notify.notifyOnCancelSearchRequest searchTry.merchantOperatingCityId driver_ driverReq.searchTryId searchTry.tripCategory
  where
    callWithErrorHandling transactionId action = do
      exep <- withTryCatch "cancelSearch:callWithErrorHandling" action
      case exep of
        Left e -> do
          logError $ "cancelSearchInitLock | cancelSearch release (error) | txn=" <> transactionId
          Redis.unlockRedis (mkCancelSearchInitLockKey transactionId)
          someExceptionToAPIErrorThrow e
        Right a -> do
          logError $ "cancelSearchInitLock | cancelSearch release (success) | txn=" <> transactionId
          Redis.unlockRedis (mkCancelSearchInitLockKey transactionId)
          pure a

    someExceptionToAPIErrorThrow exc
      | Just (HTTPException err) <- fromException exc = throwError err
      | Just (BaseException err) <- fromException exc =
        throwError . InternalError . fromMaybe (show err) $ toMessage err
      | otherwise = throwError . InternalError $ show exc

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
  searchReq <- QSRLite.findByTransactionIdAndMerchantIdLite transactionId merchantId >>= fromMaybeM (SearchRequestNotFound $ "transactionId-" <> transactionId <> ",merchantId-" <> merchantId.getId)
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
