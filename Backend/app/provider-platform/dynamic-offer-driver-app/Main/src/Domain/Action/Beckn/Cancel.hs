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
import qualified SharedLogic.Analytics as Analytics
import qualified SharedLogic.BehaviourManagement.PickupStall as PickupStall
import SharedLogic.Booking
import SharedLogic.Cancel
import qualified SharedLogic.CancellationOrchestrator as Orchestrator
import qualified SharedLogic.DriverPool as DP
import qualified SharedLogic.External.LocationTrackingService.Flow as LF
import qualified SharedLogic.External.LocationTrackingService.Types as LT
import qualified SharedLogic.MetricsLabels as SML
import SharedLogic.Ride
import qualified SharedLogic.ScheduledBooking.OverlapCheck as SBOC
import qualified SharedLogic.SearchTryLocker as CS
import qualified Storage.CachedQueries.Driver.GoHomeRequest as CQDGR
import qualified Storage.CachedQueries.Merchant as QM
import qualified Storage.CachedQueries.ValueAddNP as CQVAN
import Storage.ConfigPilot.Config.TransporterConfig (TransporterConfigDimensions (..))
import qualified Storage.Queries.Booking as QRB
import qualified Storage.Queries.BookingCancellationReason as QBCR
import qualified Storage.Queries.DriverInformation as QDI
import qualified Storage.Queries.DriverQuote as QDQ
import qualified Storage.Queries.Person as QPers
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.QueriesExtra.SearchRequestLite as QSRLite
import qualified Storage.Queries.Ride as QRide
import qualified Storage.Queries.SearchRequestForDriver as QSRD
import qualified Storage.Queries.SearchTry as QST
import qualified Storage.Queries.Vehicle as QVeh
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
      when (booking.isScheduled) $
        -- recompute the gate under the per-driver hold lock to avoid racing an accept/release
        CS.withDriverScheduledHoldLock ride.driverId $ do
          mbNextHold <- SBOC.nextScheduledHoldAfterRelease transporterConfig ride.driverId booking.id
          QDI.updateDriverInfo ride.driverId [QDI.SetLatestScheduledBooking (fst <$> mbNextHold), QDI.SetLatestScheduledPickup (snd <$> mbNextHold)]

    (disToPickup, mbLocation) <- getDistanceToPickup booking mbRide
    let currentLocation = getCoordinates <$> mbLocation
    bookingCR <- buildBookingCancellationReason disToPickup currentLocation mbRide
    QBCR.upsert bookingCR
    cityLabel <- SML.getCityLabel booking.merchantOperatingCityId
    Metrics.incrementRideCancelledCount merchant.shortId.getShortId cityLabel (show booking.vehicleServiceTier) (show bookingCR.source) (SML.distanceBucketLabel (SML.distanceBucketEdges transporterConfig) booking.estimatedDistance)
    QRB.updateStatus booking.id SRB.CANCELLED
    when booking.isScheduled $ removeBookingFromRedis booking
    -- ONE decision (signals → fault verdict → consequence-matrix row), resolved up front
    -- and Redis-cached per ride. The orchestrator then applies the immediate consequences
    -- (blacklist, driver overlay, coin event, driver money, rate counter) BEFORE the
    -- reallocation decision: a customer forced to cancel because of the driver must still
    -- produce the driver-side consequences even when the booking reallocates.
    mbConsequenceCtx <- forM mbRide $ \ride -> do
      driver <- QPers.findById ride.driverId >>= fromMaybeM (PersonNotFound ride.driverId.getId)
      decision <- Orchestrator.decideCancellationConsequences booking ride transporterConfig bookingCR.source bookingCR.reasonCode disToPickup
      let consequenceCtx = Orchestrator.ConsequenceCtx {merchant = merchant, booking = booking, ride = ride, transporterConfig = transporterConfig, driver = driver, source = bookingCR.source, decision = decision}
      Orchestrator.applyImmediateConsequences consequenceCtx Nothing
      pure consequenceCtx

    whenJust mbRide $ \ride -> do
      triggerRideCancelledEvent RideEventData {ride = ride{status = SRide.CANCELLED}, personId = ride.driverId, merchantId = merchant.id}
      triggerBookingCancelledEvent BookingEventData {booking = booking{status = SRB.CANCELLED}, personId = ride.driverId, merchantId = merchant.id}

    -- Driver-fault attribution: persist the pickup journey onto the ride, and if the
    -- monitor saw the driver in a hard fault state (STALLED / MOVING_AWAY) when the
    -- customer cancelled, count it against him. A ride with pickupBehaviour already set
    -- means monitoring ended earlier (a terminal stage recorded the stall, or the driver
    -- reached pickup) — don't double-record. Runs regardless of whether this cancel
    -- results in reallocation: a customer forced to cancel/reallocate because the driver
    -- never moved is exactly the case this must catch. Ordering: the fault verdict above
    -- already consumed the live journey, so flushing here is safe.
    whenJust mbRide $ \ride ->
      fork "record pickup stall on customer cancel" $ do
        mbJourney <- PickupStall.getPickupJourney ride
        PickupStall.flushPickupJourney ride Nothing
        when (isNothing ride.pickupBehaviour) $
          whenJust mbJourney $ \journey ->
            when (journey.behaviour `elem` [SRide.STALLED, SRide.MOVING_AWAY]) $
              PickupStall.recordPickupStall transporterConfig ride.driverId ride.merchantOperatingCityId ride.id (PickupStall.behaviourLabel journey.behaviour) PickupStall.CustomerCancelledDriverAtFault

    isReallocated <-
      case mbConsequenceCtx of
        Just consequenceCtx -> do
          let ride = consequenceCtx.ride
              driver = consequenceCtx.driver
          fork "cancelRide - Notify driver" $
            Notify.notifyOnCancel booking.merchantOperatingCityId ride.id booking driver bookingCR.source
          isValueAddNP <- CQVAN.isValueAddNP booking.bapId
          vehicle <- QVeh.findById ride.driverId >>= fromMaybeM (DriverWithoutVehicle ride.driverId.getId)
          isReallocat <- reAllocateBookingIfPossible isValueAddNP (fromMaybe False req.userReallocationEnabled) merchant booking ride driver vehicle bookingCR False
          -- newRide <- QRide.findById ride.id >>= fromMaybeM (RideDoesNotExist ride.id.getId)
          -- let cancellationFeeIfCancelled = maybe Nothing (\charges-> Just PriceAPIEntity {amount = charges, currency = booking.currency}) newRide.cancellationFeeIfCancelled
          return isReallocat
        Nothing -> return False

    -- Terminal (customer-side money) consequences apply REGARDLESS of reallocation: the
    -- matrix (fault verdict + row) alone decides whether the customer pays — a
    -- driver-at-fault row simply carries no customer deduction, while a customer-at-fault
    -- cancel can no longer dodge its consequences by having reallocation enabled. On a
    -- reallocated booking the charge is recorded as dues and collected on the next ride
    -- (the reallocation on_cancel carries no fee term).
    chargesOutcome <- case mbConsequenceCtx of
      Just consequenceCtx ->
        Orchestrator.applyTerminalConsequences
          consequenceCtx
          (\base gst -> createCancellationLedgerEntries booking consequenceCtx.ride base gst transporterConfig)
      Nothing -> pure Nothing
    logTagInfo ("bookingId-" <> getId req.bookingId) ("Cancellation reason " <> show bookingCR.source)

    if isReallocated
      then do
        return (isReallocated, Nothing, Nothing)
      else do
        let cancellationTaxAmount = fromMaybe 0 (chargesOutcome >>= (.tax))
            -- base + tax kept separate; total built only here for the on_cancel
            -- CancellationTerm. A non-positive total (zero, or a matrix CREDIT) is
            -- never surfaced as a cancellation fee on the Beckn side.
            cancelCharges = do
              base <- chargesOutcome >>= (.fee)
              let total = base + cancellationTaxAmount
              if total > 0 then Just (PriceAPIEntity {amount = total, currency = booking.currency}) else Nothing

        logTagInfo ("bookingId-" <> getId req.bookingId) ("cancellationCharges: " <> show cancelCharges)

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
