{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.UI.Ride.CancelRide.Internal
  ( cancelRideImpl,
    cancelRideTransaction,
    createCancellationLedgerEntries,
    applyCancellationLedgerAction,
    updateNammaTagsForCancelledRide,
    driverDistanceToPickup,
    getCancellationCharges,
    customerCancellationChargesCalculation,
    CancellationChargesOutcome (..),
    getDistanceToPickup,
    buildPenaltyCheckContext,
  )
where

import Data.Aeson as A
import Data.Either.Extra (eitherToMaybe)
import qualified Data.HashMap.Strict as HM
import qualified Data.HashMap.Strict as HMS
import qualified Data.Map as M
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import qualified Domain.Types.Booking as SRB
import qualified Domain.Types.BookingCancellationReason as SBCR
-- import qualified Lib.Yudhishthira.Event as Yudhishthira

-- import qualified Lib.Yudhishthira.Tools.Utils as LYTU

import qualified Domain.Types.CancellationDuesDetails as DCDD
import qualified Domain.Types.CancellationReason as DTCR
import Domain.Types.DriverLocation
import "beckn-spec" Domain.Types.Invoice (InvoiceType (..), IssuedToType (..))
import qualified Domain.Types.Merchant as DMerc
import qualified Domain.Types.MerchantPaymentMethod as DMPM
import qualified Domain.Types.Person as SP
import qualified Domain.Types.Ride as DRide
import qualified Domain.Types.RiderDetails as RiderDetails
import qualified Domain.Types.TransporterConfig as DTC
import qualified Domain.Types.VehicleVariant as Veh
import qualified Domain.Types.Yudhishthira as TY
import EulerHS.Prelude hiding (whenJust)
import Kernel.External.Maps
import Kernel.Prelude hiding (any, elem, map, mapM_, notElem)
import Kernel.Storage.Clickhouse.Config
import qualified Kernel.Storage.Clickhouse.Config as CH
import qualified Kernel.Storage.ClickhouseV2 as CHV2
import qualified Kernel.Storage.Esqueleto as Esq hiding (whenJust_)
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Streaming.Kafka.Producer.Types (HasKafkaProducer, KafkaProducerTools)
import Kernel.Types.Id
import Kernel.Types.Version (CloudType)
import Kernel.Utils.Common
import qualified Kernel.Utils.Version as Version
import Lib.ConfigPilot.Interface.Types (getOneConfig)
import qualified Lib.DriverCoins.Coins as DC
import qualified Lib.DriverCoins.Types as DCT
import qualified Lib.DriverScore as DS
import qualified Lib.DriverScore.Types as DST
import Lib.Finance (AccountRole (..), EntryStatus (..), FinanceCtx, InvoiceConfig (..), InvoiceLineItem (..), ItemType (..), LineItemDescription (..), createReversal, getEntriesByReference, invoice, runFinance, settleEntry, transfer, transferPending, transferWithoutAttribution, transfer_, voidEntry)
import qualified Lib.Finance.Core.Types as Finance
import Lib.Scheduler (SchedulerType)
import Lib.SessionizerMetrics.Types.Event
import qualified Lib.Yudhishthira.Tools.DebugLog as LYDL
import qualified Lib.Yudhishthira.Types as LYT
import qualified Lib.Yudhishthira.Types as Yudhishthira
import qualified SharedLogic.Analytics as Analytics
import qualified SharedLogic.CallBAP as BP
import SharedLogic.CallBAPInternal
import qualified SharedLogic.CallInternalMLPricing as ML
import SharedLogic.Cancel
import qualified SharedLogic.DriverCancellationPenalty as DCP
import qualified SharedLogic.External.LocationTrackingService.Flow as LF
import qualified SharedLogic.External.LocationTrackingService.Types as LT
import SharedLogic.Finance.GstBreakdown
import SharedLogic.Finance.Wallet
import SharedLogic.GoogleTranslate (TranslateFlow)
import SharedLogic.Ride (releaseLien, updateOnRideStatusWithAdvancedRideCheck)
import SharedLogic.RuleBasedTierUpgrade
import qualified SharedLogic.SpecialZoneDriverDemand as SpecialZoneDriverDemand
import qualified SharedLogic.UserCancellationDues as UserCancellationDues
import qualified Storage.Cac.TransporterConfig as SCTC
import qualified Storage.CachedQueries.Driver.GoHomeRequest as CQDGR
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.CachedQueries.Merchant.MerchantPaymentMethod as CQMPM
import qualified Storage.CachedQueries.ValueAddNP as CQVAN
import Storage.ConfigPilot.Config.TransporterConfig (TransporterConfigDimensions (..))
import qualified Storage.Queries.Booking as QRB
import qualified Storage.Queries.BookingCancellationReason as QBCR
import qualified Storage.Queries.CallStatus as QCallStatus
import qualified Storage.Queries.CancellationDuesDetails as QCDD
import qualified Storage.Queries.DriverInformation as QDI
import qualified Storage.Queries.DriverPanCard as QPanCard
import qualified Storage.Queries.DriverQuote as QDQ
import qualified Storage.Queries.DriverStats as QDriverStats
import qualified Storage.Queries.FareParameters as QFP
import qualified Storage.Queries.FleetOwnerInformation as QFOI
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.QueriesExtra.SearchRequestLite as QSRLite
import qualified Storage.Queries.Ride as QRide
import qualified Storage.Queries.RideDetails as QRideDetails
import qualified Storage.Queries.RiderDetails as QRiderDetails
import qualified Storage.Queries.Vehicle as QVeh
import Tools.Constants
import Tools.DynamicLogic
import Tools.Error
import Tools.Event
import qualified Tools.Maps as Maps
import qualified Tools.Metrics as Metrics
import qualified Tools.Notifications as Notify
import TransactionLogs.Types

-- main fn
cancelRideImpl ::
  ( EncFlow m r,
    EsqDBReplicaFlow m r,
    CacheFlow m r,
    EsqDBFlow m r,
    HasKafkaProducer r,
    HasField "searchRequestExpirationSeconds" r NominalDiffTime,
    HasField "jobInfoMap" r (M.Map Text Bool),
    HasField "maxShards" r Int,
    HasField "schedulerSetName" r Text,
    HasField "schedulerType" r SchedulerType,
    Metrics.HasSendSearchRequestToDriverMetrics m r,
    HasFlowEnv m r '["kafkaProducerTools" ::: KafkaProducerTools],
    HasHttpClientOptions r c,
    HasLongDurationRetryCfg r c,
    HasField "singleBatchProcessingTempDelay" r NominalDiffTime,
    HasFlowEnv m r '["internalEndPointHashMap" ::: HM.HashMap BaseUrl BaseUrl],
    HasFlowEnv m r '["ondcTokenHashMap" ::: HMS.HashMap KeyConfig TokenConfig],
    HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    HasFlowEnv m r '["ltsCfg" ::: LT.LocationTrackingeServiceConfig],
    HasFlowEnv m r '["cloudType" ::: Maybe CloudType],
    TranslateFlow m r,
    LT.HasLocationService m r,
    HasFlowEnv m r '["maxNotificationShards" ::: Int],
    HasShortDurationRetryCfg r c,
    Redis.HedisFlow m r,
    EventStreamFlow m r,
    Metrics.HasCoreMetrics r,
    HasShortDurationRetryCfg r c,
    HasField "enableAPILatencyLogging" r Bool,
    HasField "enableAPIPrometheusMetricLogging" r Bool,
    HasFlowEnv m r '["appBackendBapInternal" ::: AppBackendBapInternal],
    HasFlowEnv m r '["mlPricingInternal" ::: ML.MLPricingInternal],
    HasField "serviceClickhouseCfg" r CH.ClickhouseCfg,
    HasField "serviceClickhouseEnv" r CH.ClickhouseEnv,
    CHV2.HasClickhouseEnv CHV2.APP_SERVICE_CLICKHOUSE m,
    HasField "blackListedJobs" r [Text],
    HasField "enableLtsPoolDataForPooling" r Bool,
    Redis.HedisLTSFlowEnv r,
    ClickhouseFlow m r,
    Finance.HasActorInfo m r
  ) =>
  Id DRide.Ride ->
  DRide.RideEndedBy ->
  SBCR.BookingCancellationReason ->
  Bool ->
  Maybe Bool ->
  Bool ->
  m ()
cancelRideImpl rideId rideEndedBy bookingCReason isForceReallocation doCancellationRateBasedBlocking allowSnapshotVehicleFallback = do
  isLocked <- Redis.tryLockRedis (buildCancelRideTransactionKey rideId) 15
  if isLocked
    then do
      finally
        ( do
            ride <- QRide.findById rideId >>= fromMaybeM (RideDoesNotExist rideId.getId)
            booking <- QRB.findById ride.bookingId >>= fromMaybeM (BookingNotFound ride.bookingId.getId)
            isValueAddNP <- CQVAN.isValueAddNP booking.bapId
            let merchantId = booking.providerId
            merchant <-
              CQM.findById merchantId
                >>= fromMaybeM (MerchantNotFound merchantId.getId)
            transporterConfig <- getOneConfig (TransporterConfigDimensions {merchantOperatingCityId = booking.merchantOperatingCityId.getId}) (Just (SCTC.findByMerchantOpCityId booking.merchantOperatingCityId Nothing)) >>= fromMaybeM (TransporterConfigNotFound booking.merchantOperatingCityId.getId)
            rideTags <- updateNammaTagsForCancelledRide booking ride bookingCReason transporterConfig
            noShowCharges <- withTryCatch "noShowCharges:cancelRideImpl" $ do
              if transporterConfig.canAddCancellationFee
                then do
                  let tagsForCancellationCharges = [validCustomerCancellation, validUserNoShowCancellation]
                      cancellationType = DCT.CancellationByDriver
                  if any (`elem` rideTags) tagsForCancellationCharges
                    then getCancellationCharges booking ride cancellationType bookingCReason.reasonCode
                    else return (Nothing, Nothing)
                else return (Nothing, Nothing)
            userNoShowCharges <- case noShowCharges of
              Left e -> do
                logError $ "Error in getting no show charges - " <> show e
                return (Nothing, Nothing)
              Right charges -> return charges
            logDebug $ "userNoShowCharges: " <> show userNoShowCharges
            let (userNoShowOutcome, userNoShowChargesLogicVersion) = userNoShowCharges
            driver <- QPerson.findById ride.driverId >>= fromMaybeM (PersonNotFound ride.driverId.getId)
            mbVehicle <- QVeh.findById ride.driverId
            vehicle <- case mbVehicle of
              Just v -> pure v
              Nothing
                | allowSnapshotVehicleFallback -> do
                  logWarning $ "Vehicle missing for driver " <> ride.driverId.getId <> " on cancelled ride " <> ride.id.getId <> "; using ride_details snapshot (ops cancel)"
                  rideDetails <- QRideDetails.findById ride.id >>= fromMaybeM (RideNotFound ride.id.getId)
                  pure $ BP.buildVehicleFromRideDetailsSnapshot booking ride rideDetails
                | otherwise -> throwError (DriverWithoutVehicle ride.driverId.getId)
            unless (isValidRide ride) $ throwError (InternalError "Ride is not valid for cancellation")
            cancelRideTransaction booking ride bookingCReason merchant rideEndedBy userNoShowOutcome userNoShowChargesLogicVersion transporterConfig driver
            logTagInfo ("rideId-" <> getId rideId) ("Cancellation reason " <> show bookingCReason.source)
            -- Release pickup-zone counters (idempotent). ByDriver triggers reallocation,
            -- so demand stays live for the next match; every other source terminates the booking.
            -- Supply for the assigned driver's pickup-zone request is released regardless.
            fork "specialZoneCountersReleaseOnCancel" $
              SpecialZoneDriverDemand.releasePickupZoneCountersOnCancel
                (bookingCReason.source == SBCR.ByDriver)
                booking.id.getId
                booking.pickupGateId
                (show $ Veh.castServiceTierToVariant booking.vehicleServiceTier)
                (Just ride.driverId)

            fork "DriverRideCancelledCoin Event : " $ do
              mbLocation <- do
                driverLocations <- LF.driversLocationByCloudType [ride.driverId] driver.cloudType
                return $ listToMaybe driverLocations
              disToPickup <- forM mbLocation $ \location -> do
                driverDistanceToPickup booking (getCoordinates location) (getCoordinates booking.fromLocation)
              when (bookingCReason.source == SBCR.ByDriver) $
                DC.driverCoinsEvent ride.driverId Nothing driver.merchantId booking.merchantOperatingCityId (DCT.Cancellation ride.createdAt booking.distanceToPickup disToPickup DCT.CancellationByDriver (fromMaybe (DTCR.CancellationReasonCode "OTHER") bookingCReason.reasonCode)) (Just ride.id.getId) ride.vehicleVariant (Just booking.vehicleServiceTier) (Just booking.configInExperimentVersions)

            fork "cancelRide - Notify driver" $ do
              triggerRideCancelledEvent RideEventData {ride = ride{status = DRide.CANCELLED}, personId = driver.id, merchantId = merchantId}
              triggerBookingCancelledEvent BookingEventData {booking = booking{status = SRB.CANCELLED}, personId = driver.id, merchantId = merchantId}
              when (bookingCReason.source == SBCR.ByDriver) $ do
                DS.driverScoreEventHandler ride.merchantOperatingCityId DST.OnDriverCancellation {rideTags, merchantId = merchantId, driver = driver, rideFare = Just booking.estimatedFare, currency = booking.currency, distanceUnit = booking.distanceUnit, doCancellationRateBasedBlocking}
                DCP.accumulateCancellationPenalty (fromMaybe False merchant.prepaidSubscriptionAndWalletEnabled || transporterConfig.driverWalletConfig.enableDriverWallet) booking ride rideTags transporterConfig driver
              Notify.notifyOnCancel ride.merchantOperatingCityId ride.id booking driver bookingCReason.source
            fork "cancelRide/ReAllocate - Notify BAP" $ do
              isReallocated <- reAllocateBookingIfPossible isValueAddNP False merchant booking ride driver vehicle bookingCReason isForceReallocation
              unless isReallocated $ do
                -- Reload ride to get persisted cancellationFee/cancellationFeeTax
                updatedRide <- QRide.findById ride.id
                BP.sendBookingCancelledUpdateToBAP booking merchant bookingCReason.source (userNoShowOutcome >>= (.fee)) (userNoShowOutcome >>= (.tax)) updatedRide
            computeEligibleUpgradeTiers ride transporterConfig
        )
        ( do
            logDebug $ "CancelRideTransaction:RID:-" <> rideId.getId <> " Unlocked"
            Redis.unlockRedis (buildCancelRideTransactionKey rideId)
        )
    else throwError (InternalError "Ride is already cancelled")
  where
    buildCancelRideTransactionKey rideId' = "CancelRideTransaction:RID:-" <> rideId'.getId
    isValidRide ride = ride.status `elem` [DRide.NEW, DRide.UPCOMING]

cancelRideTransaction ::
  ( EsqDBFlow m r,
    CacheFlow m r,
    Esq.EsqDBReplicaFlow m r,
    LT.HasLocationService m r,
    HasShortDurationRetryCfg r c,
    EncFlow m r,
    Redis.HedisLTSFlowEnv r,
    Finance.HasActorInfo m r
  ) =>
  SRB.Booking ->
  DRide.Ride ->
  SBCR.BookingCancellationReason ->
  DMerc.Merchant ->
  DRide.RideEndedBy ->
  Maybe CancellationChargesOutcome ->
  Maybe Int ->
  DTC.TransporterConfig ->
  SP.Person ->
  m ()
cancelRideTransaction booking ride bookingCReason merchant rideEndedBy mbChargesOutcome cancellationChargesLogicVersion transporterConfig _driver = do
  let driverId = cast ride.driverId
      isPrepaidSubscriptionAndWalletEnabled = fromMaybe False merchant.prepaidSubscriptionAndWalletEnabled
  when isPrepaidSubscriptionAndWalletEnabled $ releaseLien booking ride
  void $ CQDGR.setDriverGoHomeIsOnRideStatus ride.driverId booking.merchantOperatingCityId False
  updateOnRideStatusWithAdvancedRideCheck driverId (Just ride)
  when booking.isScheduled $ QDI.updateLatestScheduledBookingAndPickup Nothing Nothing driverId
  void $ LF.rideDetails ride.id DRide.CANCELLED merchant.id ride.driverId booking.fromLocation.lat booking.fromLocation.lon Nothing (Just $ (LT.Car $ LT.CarRideInfo {pickupLocation = LatLong (booking.fromLocation.lat) (booking.fromLocation.lon), minDistanceBetweenTwoPoints = Nothing, rideStops = Just $ map (\stop -> LatLong stop.lat stop.lon) booking.stops}))
  void $ QRide.updateStatusAndRideEndedBy ride.id DRide.CANCELLED rideEndedBy
  QBCR.upsert bookingCReason
  void $ QRB.updateStatus booking.id SRB.CANCELLED
  when (bookingCReason.source == SBCR.ByDriver) $ QDriverStats.updateIdleTime driverId
  case (mbChargesOutcome >>= (.fee), booking.riderId) of
    (Just fee, Just rid) -> do
      -- Persist charges-on-cancel total + logic version on ride; the fee/tax/overdue
      -- breakdown lives on CancellationDuesDetails. fee is the base (tax-exclusive).
      let gstOnCancellation = fromMaybe 0 (mbChargesOutcome >>= (.tax))
          baseCancellation = fee
          totalCancellation = baseCancellation + gstOnCancellation
      QRide.updateCancellationChargesOnCancel (Just totalCancellation) cancellationChargesLogicVersion ride.id
      riderDetails <- QRiderDetails.findById rid >>= fromMaybeM (RiderDetailsNotFound rid.getId)
      void $ QRiderDetails.updateCancellationDues (totalCancellation + riderDetails.cancellationDues) rid
      QRiderDetails.updateValidCancellationsCount rid.getId
      -- Track cancellation dues per ride for waive-off correctness
      when (totalCancellation > 0) $ do
        cancellationDuesDetailsId <- generateGUID
        now <- getCurrentTime
        let cancellationDuesDetails =
              DCDD.CancellationDuesDetails
                { id = cancellationDuesDetailsId,
                  rideId = ride.id,
                  riderId = rid,
                  cancellationAmount = totalCancellation,
                  cancellationFee = Just baseCancellation,
                  cancellationFeeTax = Just gstOnCancellation,
                  overdueCancellationCharge = mbChargesOutcome >>= (.overdueFee),
                  overdueCancellationTax = mbChargesOutcome >>= (.overdueTax),
                  cancellationCommission = mbChargesOutcome >>= (.commission),
                  overdueCancellationCommission = mbChargesOutcome >>= (.overdueCommission),
                  currency = booking.currency,
                  paymentStatus = DCDD.PENDING,
                  createdAt = now,
                  updatedAt = now,
                  merchantId = ride.merchantId,
                  merchantOperatingCityId = Just ride.merchantOperatingCityId
                }
        QCDD.create cancellationDuesDetails
      -- Customer cancellation ledger entries (wallet path)
      when ((isPrepaidSubscriptionAndWalletEnabled || transporterConfig.driverWalletConfig.enableDriverWallet) && totalCancellation > 0) $
        createCancellationLedgerEntries booking ride baseCancellation gstOnCancellation transporterConfig
    _ -> do
      logError "cancelRideTransaction: riderId in booking or cancellationFee is not present"

updateNammaTagsForCancelledRide ::
  ( EsqDBFlow m r,
    CacheFlow m r,
    Esq.EsqDBReplicaFlow m r,
    Redis.HedisFlow m r,
    HasField "serviceClickhouseCfg" r CH.ClickhouseCfg,
    HasField "serviceClickhouseEnv" r CH.ClickhouseEnv,
    CHV2.HasClickhouseEnv CHV2.APP_SERVICE_CLICKHOUSE m,
    ClickhouseFlow m r
  ) =>
  SRB.Booking ->
  DRide.Ride ->
  SBCR.BookingCancellationReason ->
  DTC.TransporterConfig ->
  m [LYT.TagNameValue]
updateNammaTagsForCancelledRide booking ride bookingCReason transporterConfig = do
  now <- getCurrentTime
  mbCallStatus <- QCallStatus.findOneByEntityId (Just ride.id.getId)
  let callAtemptByDriver = isJust mbCallStatus
      currentTime = floor $ utcTimeToPOSIXSeconds now
      rideCreatedTime = floor $ utcTimeToPOSIXSeconds ride.createdAt
      driverArrivalTime = floor . utcTimeToPOSIXSeconds <$> (ride.driverArrivalTime)
      bookingCreatedTime = floor $ utcTimeToPOSIXSeconds booking.createdAt
      tagData =
        TY.CancelRideTagData
          { ride = ride{status = DRide.CANCELLED},
            booking = booking{status = SRB.CANCELLED},
            cancellationReason = bookingCReason,
            merchantOperatingCityId = booking.merchantOperatingCityId,
            ..
          }
  nammaTags <- withTryCatch "computeNammaTags:RideCancel" (LYDL.computeNammaTagsWithDebugLog LYDL.Driver (cast booking.merchantOperatingCityId) Yudhishthira.RideCancel (Just booking.transactionId) tagData)
  logDebug $ "Tags for cancelled ride, rideId: " <> ride.id.getId <> " tagresults:" <> show (eitherToMaybe nammaTags) <> "| tagdata: " <> show tagData
  let allTags = ride.rideTags <> eitherToMaybe nammaTags
  QRide.updateRideTags allTags ride.id
  let tags = fromMaybe [] allTags
  when (maybe False (`elem` validCancellationPenaltyReasonCodes transporterConfig) bookingCReason.reasonCode && validUserNoShowCancellation `notElem` tags) $
    logError $ "Customer no show tag was not applied: rideId: " <> ride.id.getId
  Analytics.updateCancellationAnalyticsAndDriverStats transporterConfig ride bookingCReason
  return $ fromMaybe [] allTags

driverDistanceToPickup ::
  ( EncFlow m r,
    CacheFlow m r,
    EsqDBFlow m r,
    Maps.HasCoordinates tripStartPos,
    Maps.HasCoordinates tripEndPos,
    ToJSON tripStartPos,
    ToJSON tripEndPos,
    HasKafkaProducer r
  ) =>
  SRB.Booking ->
  tripStartPos ->
  tripEndPos ->
  m Meters
driverDistanceToPickup booking tripStartPos tripEndPos = do
  distRes <-
    Maps.getDistanceForCancelRide booking.providerId booking.merchantOperatingCityId (Just booking.id.getId) $
      Maps.GetDistanceReq
        { origin = tripStartPos,
          destination = tripEndPos,
          travelMode = Just Maps.CAR,
          distanceUnit = booking.distanceUnit,
          sourceDestinationMapping = Nothing
        }
  return $ distRes.distance

getDistanceToPickup ::
  ( EsqDBFlow m r,
    CacheFlow m r,
    HasField "shortDurationRetryCfg" r RetryCfg,
    EncFlow m r,
    HasKafkaProducer r,
    HasFlowEnv m r '["ltsCfg" ::: LT.LocationTrackingeServiceConfig],
    HasFlowEnv m r '["cloudType" ::: Maybe CloudType],
    Esq.EsqDBReplicaFlow m r
  ) =>
  SRB.Booking ->
  Maybe DRide.Ride ->
  m (Maybe Meters, Maybe DriverLocation)
getDistanceToPickup booking mbRide = do
  case mbRide of
    Just ride -> do
      mbDriver <- QPerson.findById ride.driverId
      mbLocation <- do
        driverLocations <- withTryCatch "driversLocation:getDistanceToPickup" $ LF.driversLocationByCloudType [ride.driverId] (mbDriver >>= (.cloudType))
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

-- | Cancellation charge outcome from the USER_CANCELLATION_DUES logic: the fee/tax pair, the
--   reduced overdue variants, and the platform's commission on each. Every field is optional —
--   a rule that emits no value for a field decodes to Nothing, and a Nothing fee means no charge
--   applies (downstream skips the dues/counter writes).
data CancellationChargesOutcome = CancellationChargesOutcome
  { fee :: Maybe HighPrecMoney,
    tax :: Maybe HighPrecMoney,
    overdueFee :: Maybe HighPrecMoney,
    overdueTax :: Maybe HighPrecMoney,
    commission :: Maybe HighPrecMoney,
    overdueCommission :: Maybe HighPrecMoney
  }
  deriving (Generic, Show)

customerCancellationChargesCalculation ::
  ( EsqDBFlow m r,
    CacheFlow m r,
    EncFlow m r,
    HasKafkaProducer r,
    HasField "shortDurationRetryCfg" r RetryCfg,
    HasFlowEnv m r '["ltsCfg" ::: LT.LocationTrackingeServiceConfig],
    HasFlowEnv m r '["cloudType" ::: Maybe CloudType],
    Esq.EsqDBReplicaFlow m r,
    HasField "serviceClickhouseCfg" r CH.ClickhouseCfg,
    HasField "serviceClickhouseEnv" r CH.ClickhouseEnv,
    CHV2.HasClickhouseEnv CHV2.APP_SERVICE_CLICKHOUSE m,
    ClickhouseFlow m r
  ) =>
  SRB.Booking ->
  DRide.Ride ->
  RiderDetails.RiderDetails ->
  DCT.CancellationType ->
  Maybe DTCR.CancellationReasonCode ->
  Maybe Int ->
  m (Maybe CancellationChargesOutcome, Maybe Int)
customerCancellationChargesCalculation booking ride riderDetails cancellationType reasonCode mbExistingVersion = do
  transporterConfig <- getOneConfig (TransporterConfigDimensions {merchantOperatingCityId = booking.merchantOperatingCityId.getId}) (Just (SCTC.findByMerchantOpCityId booking.merchantOperatingCityId Nothing)) >>= fromMaybeM (TransporterConfigNotFound booking.merchantOperatingCityId.getId)
  (cancellationDisToPickup, _mbLocation) <- getDistanceToPickup booking (Just ride)
  now <- getCurrentTime
  durationToPickup <- (maybe (fromMaybe 0 booking.dqDurationToPickup) (.durationToPickup)) <$> (QDQ.findById (Id booking.quoteId))
  let estimatedTimeToPickup = secondsToNominalDiffTime durationToPickup
  mbCallStatus <- QCallStatus.findOneByEntityId (Just ride.id.getId)
  let callAttemptByDriver = isJust mbCallStatus
  let _isArrivedAtPickup = case cancellationDisToPickup of
        Just disToPickup' -> disToPickup' < highPrecMetersToMeters transporterConfig.arrivedPickupThreshold
        Nothing -> False
      timeOfCancellation = round $ diffUTCTime now ride.createdAt
      initialDisToPickup = booking.distanceToPickup
      actualCoveredDistance = case (initialDisToPickup, cancellationDisToPickup) of
        (Just initial, Just cancellation) -> Just (initial - cancellation)
        _ -> Nothing
      expectedCoveredDistance =
        if isJust initialDisToPickup
          then
            let initialDistance = fromJust initialDisToPickup
                progressRatio = fromIntegral timeOfCancellation / max 1 estimatedTimeToPickup
                expectedDistance = round $ fromIntegral initialDistance * progressRatio
             in Just expectedDistance
          else Nothing
      driverWaitingTime = if isJust ride.driverArrivalTime then Just (round $ diffUTCTime now (fromJust ride.driverArrivalTime)) else Nothing
  mbSearchRequest <- QSRLite.findByTransactionIdAndMerchantIdLite booking.transactionId booking.providerId
  let userSdkVersionText = Version.versionToText <$> (mbSearchRequest >>= (.userSdkVersion))
  mbPaymentMethod <- forM booking.paymentMethodId $ \pmId ->
    CQMPM.findByIdAndMerchantOpCityId pmId booking.merchantOperatingCityId
      >>= fromMaybeM (MerchantPaymentMethodNotFound pmId.getId)
  let isCashPayment = case mbPaymentMethod of
        Nothing -> True
        Just pm -> pm.paymentInstrument == DMPM.Cash
      -- Payment-method exception: skip the cancellation fee entirely for configured instruments (e.g. Cash).
      -- A booking without a payment method is treated as Cash, matching isCashPayment above.
      bookingPaymentInstrument = maybe DMPM.Cash (.paymentInstrument) mbPaymentMethod
      isCancellationFeeExemptPaymentMethod = bookingPaymentInstrument `elem` fromMaybe [] transporterConfig.cancellationFeePaymentMethodExceptions
  let timeSinceBooking = round $ diffUTCTime now booking.createdAt
      logicInput =
        UserCancellationDues.UserCancellationDuesData
          { cancelledBy = cancellationType,
            timeOfDriverCancellation = timeOfCancellation,
            timeOfCustomerCancellation = timeOfCancellation,
            isArrivedAtPickup = isJust ride.driverArrivalTime || _isArrivedAtPickup,
            driverWaitingTime = fromMaybe 0 driverWaitingTime,
            callAttemptByDriver = callAttemptByDriver,
            actualCoveredDistance = fromMaybe 0 actualCoveredDistance,
            expectedCoveredDistance = fromMaybe 0 expectedCoveredDistance,
            cancellationDues = riderDetails.cancellationDues,
            cancelledRides = riderDetails.cancelledRides,
            totalBookings = riderDetails.totalBookings,
            completedRides = riderDetails.completedRides,
            estimatedBookingFare = booking.estimatedFare,
            validCancellations = riderDetails.validCancellations,
            cancellationDueRides = riderDetails.cancellationDueRides,
            serviceTier = booking.vehicleServiceTier,
            tripCategory = booking.tripCategory,
            cancellationReasonSelected = reasonCode,
            userSdkVersion = userSdkVersionText,
            isCashPayment = isCashPayment,
            timeSinceBooking = timeSinceBooking
          }
  if transporterConfig.canAddCancellationFee && not isCancellationFeeExemptPaymentMethod
    then do
      localTime <- getLocalCurrentTime transporterConfig.timeDiffFromUtc
      (allLogics, mbVersion) <- getAppDynamicLogic (cast ride.merchantOperatingCityId) LYT.USER_CANCELLATION_DUES localTime mbExistingVersion Nothing
      logDebug $ "allLogics: for cancellation charges calculation" <> show allLogics
      logDebug $ "logicInput: for cancellation charges calculation" <> show logicInput
      response <- withTryCatch "runLogics:UserCancellationDues" $ LYDL.runLogicsWithDebugLog LYDL.Driver (cast ride.merchantOperatingCityId) LYT.USER_CANCELLATION_DUES (Just booking.transactionId) allLogics logicInput
      outcome <- case response of
        Left e -> do
          logError $ "Error in running UserCancellationDuesLogics - " <> show e <> " - " <> show logicInput <> " - " <> show allLogics
          return CancellationChargesOutcome {fee = Just 0, tax = Just 0, overdueFee = Nothing, overdueTax = Nothing, commission = Nothing, overdueCommission = Nothing}
        Right resp -> do
          case (A.fromJSON resp.result :: Result UserCancellationDues.UserCancellationDuesResult) of
            A.Success result -> do
              logTagInfo ("bookingId-" <> getId booking.id) ("result.cancellationCharges: " <> show result.cancellationCharges <> " tax: " <> show result.cancellationChargesTax <> " overdue: " <> show result.overdueCancellationCharge <> " overdueTax: " <> show result.overdueCancellationTax <> " commission: " <> show result.cancellationCommission <> " overdueCommission: " <> show result.overdueCancellationCommission)
              return
                CancellationChargesOutcome
                  { fee = Just result.cancellationCharges,
                    tax = result.cancellationChargesTax,
                    overdueFee = result.overdueCancellationCharge,
                    overdueTax = result.overdueCancellationTax,
                    commission = result.cancellationCommission,
                    overdueCommission = result.overdueCancellationCommission
                  }
            A.Error e -> do
              logError $ "Error in parsing UserCancellationDuesResult - " <> show e <> " - " <> show resp.result <> " - " <> show logicInput <> " - " <> show allLogics
              return CancellationChargesOutcome {fee = Just 0, tax = Nothing, overdueFee = Nothing, overdueTax = Nothing, commission = Nothing, overdueCommission = Nothing}
      when (maybe False (`elem` validCancellationPenaltyReasonCodes transporterConfig) reasonCode && fromMaybe 0 outcome.fee <= 0) $
        logError $ "User no show charges was not applied: " <> show outcome.fee <> ": rideId: " <> ride.id.getId <> ". Please check dynamic logic"
      -- Commission outputs are controlled by the JsonLogic (Maybe commission /
      -- overdueCommission) directly — no separate enable flag needed.
      pure (Just outcome, mbVersion)
    else return (Nothing, Nothing)

-- | Default no-show cancellation reason, used when transporter_config leaves the penalty reasons unset.
userNoShowCancellationReason :: DTCR.CancellationReasonCode
userNoShowCancellationReason = DTCR.CancellationReasonCode "CUSTOMER_NO_SHOW"

-- | Cancellation reason codes that qualify a driver cancellation as a penalizable no-show.
-- Configurable per operating city via transporter_config.validCancellationPenaltyReasons;
-- falls back to CUSTOMER_NO_SHOW when unset.
validCancellationPenaltyReasonCodes :: DTC.TransporterConfig -> [DTCR.CancellationReasonCode]
validCancellationPenaltyReasonCodes transporterConfig =
  maybe [userNoShowCancellationReason] (map DTCR.CancellationReasonCode) transporterConfig.validCancellationPenaltyReasons

getCancellationCharges ::
  ( EsqDBFlow m r,
    CacheFlow m r,
    EncFlow m r,
    HasKafkaProducer r,
    Esq.EsqDBReplicaFlow m r,
    HasField "shortDurationRetryCfg" r RetryCfg,
    HasFlowEnv m r '["ltsCfg" ::: LT.LocationTrackingeServiceConfig],
    HasFlowEnv m r '["cloudType" ::: Maybe CloudType],
    HasField "serviceClickhouseCfg" r CH.ClickhouseCfg,
    HasField "serviceClickhouseEnv" r CH.ClickhouseEnv,
    CHV2.HasClickhouseEnv CHV2.APP_SERVICE_CLICKHOUSE m,
    ClickhouseFlow m r
  ) =>
  SRB.Booking ->
  DRide.Ride ->
  DCT.CancellationType ->
  Maybe DTCR.CancellationReasonCode ->
  m (Maybe CancellationChargesOutcome, Maybe Int)
getCancellationCharges booking ride cancellationType reasonCode = do
  transporterConfig <- getOneConfig (TransporterConfigDimensions {merchantOperatingCityId = booking.merchantOperatingCityId.getId}) (Just (SCTC.findByMerchantOpCityId booking.merchantOperatingCityId Nothing)) >>= fromMaybeM (TransporterConfigNotFound booking.merchantOperatingCityId.getId)
  case booking.riderId of
    Nothing -> return (Nothing, Nothing)
    Just rid -> do
      riderDetails <- QRiderDetails.findById rid >>= fromMaybeM (RiderDetailsNotFound rid.getId)
      if transporterConfig.canAddCancellationFee
        then do
          (mbOutcome, mbVersion) <- customerCancellationChargesCalculation booking ride riderDetails cancellationType reasonCode ride.cancellationChargesLogicVersion
          -- A computed-but-zero total charge means "nothing to collect": drop the fee (and with it
          -- the commission) so downstream skips the dues/counter writes, but keep the overdue fields.
          let dropZeroCharge o = case o.fee of
                Just charges
                  | charges + fromMaybe 0 o.tax == 0 ->
                    CancellationChargesOutcome {fee = Nothing, tax = Nothing, overdueFee = o.overdueFee, overdueTax = o.overdueTax, commission = Nothing, overdueCommission = o.overdueCommission}
                _ -> o
          return (dropZeroCharge <$> mbOutcome, mbVersion)
        else return (Nothing, Nothing)

-- | Create BPP-side finance ledger entries + invoice for a customer cancellation charge.
-- Extracted so it can be called from both cancelRideTransaction (driver-cancel path)
-- and Domain.Action.Beckn.Cancel (rider-cancel via Beckn path).
createCancellationLedgerEntries ::
  ( EsqDBFlow m r,
    CacheFlow m r,
    EncFlow m r,
    Finance.HasActorInfo m r
  ) =>
  SRB.Booking ->
  DRide.Ride ->
  HighPrecMoney ->
  HighPrecMoney ->
  DTC.TransporterConfig ->
  m ()
createCancellationLedgerEntries booking ride baseCancellation gstOnCancellation transporterConfig = do
  let riderId = booking.riderId
  case riderId of
    Nothing -> logError "createCancellationLedgerEntries: riderId not present in booking"
    Just rid -> do
      let driverOrFleetPersonId = fromMaybe ride.driverId ride.fleetOwnerId
      mbPanCard <- QPanCard.findByDriverId driverOrFleetPersonId
      driver <- QPerson.findById ride.driverId >>= fromMaybeM (PersonNotFound ride.driverId.getId)
      mbDriverInfo <- QDI.findById (cast ride.driverId)
      -- Read the materialized tds_rate for the tax subject (fleet owner if it's
      -- a fleet ride, else the driver). Set by the PAN / linkage webhooks when
      -- PAN-Aadhaar-link TDS is enabled (see PanVerification.materializeTdsRateFor).
      mbStoredTdsRate <- case ride.fleetOwnerId of
        Just fleetOwnerId -> do
          mbFleetInfo <- QFOI.findByPrimaryKey (cast fleetOwnerId)
          pure (mbFleetInfo >>= (.tdsRate))
        Nothing -> pure (mbDriverInfo >>= (.tdsRate))
      mbCumulativeEarnings <- case ride.fleetOwnerId of
        Just _ -> pure Nothing
        Nothing -> do
          mbStats <- QDriverStats.findByPrimaryKey (cast ride.driverId)
          pure $ (.totalEarnings) <$> mbStats
      let rideGst = transporterConfig.taxConfig.rideGst
          cancelIsVat = fromMaybe False booking.fareParams.isVatTaxType
          -- VAT stays with the driver (OwnerLiability), GST is remitted to govt (GovtIndirect) — mirrors createDriverWalletTransaction.
          cancellationTaxDest = if cancelIsVat then OwnerLiability else GovtIndirect
          cancellationComponents =
            [ (baseCancellation, walletReferenceCustomerCancellationCharges, OwnerLiability),
              (gstOnCancellation, walletReferenceCustomerCancellationGST, cancellationTaxDest)
            ]
          mbTdsRate =
            if panAadhaarLinkTdsEnabled transporterConfig.taxConfig
              then computeEffectiveTdsRate mbPanCard mbStoredTdsRate transporterConfig.taxConfig
              else (.rate) <$> transporterConfig.taxConfig.defaultTdsRate
          mbTdsAmount = do
            rate <- mbTdsRate
            let rawAmount = baseCancellation * realToFrac rate
                gatedAmount = applyThresholdBenefit transporterConfig.taxConfig mbCumulativeEarnings mbPanCard baseCancellation rawAmount
            if gatedAmount > 0 then Just gatedAmount else Nothing
      -- Resolve rider's payment-mode choice from booking.paymentMethodId — same logic as EndRide.
      -- Cash → "CASH", anything else (Card/UPI/Wallet/NetBanking/BoothOnline) → "ONLINE".
      isOnline <- do
        let forceOnline = fromMaybe False transporterConfig.driverWalletConfig.forceOnlineLedger
        if forceOnline
          then pure True
          else do
            mbPaymentMethod <- forM booking.paymentMethodId $ \paymentMethodId ->
              CQMPM.findByIdAndMerchantOpCityId paymentMethodId booking.merchantOperatingCityId
                >>= fromMaybeM (MerchantPaymentMethodNotFound paymentMethodId.getId)
            case mbPaymentMethod of
              Nothing -> pure False
              Just paymentMethod -> case paymentMethod.paymentInstrument of
                DMPM.Cash -> pure False
                _ -> pure True
      ctx <- buildFinanceCtx booking ride (Just driver) mbPanCard mbDriverInfo transporterConfig True
      rideGstBreakdown <-
        computeGstBreakdownForRideOwner
          rideGst
          booking.fromLocation
          ride.fleetOwnerId
          ride.driverId
          gstOnCancellation
      result <- runFinance ctx $ do
        mapM_
          ( \(amt, ref, dest) -> do
              -- Two legs through BuyerExternal (nets to 0), mirroring the online ride-payment ledger.
              void $ transferPending BuyerAsset BuyerExternal amt ref
              void $ transferPending BuyerExternal dest amt ref
          )
          cancellationComponents
        whenJust mbTdsAmount $ \tdsAmount ->
          void $ transferPending OwnerLiability GovtDirect tdsAmount walletReferenceTDSDeductionCancellation
        invoice
          InvoiceConfig
            { invoiceType = RideCancellation,
              issuedToType = CUSTOMER,
              issuedToId = rid.getId,
              issuedToName = booking.riderName,
              issuedToAddress = booking.fromLocation.address.fullAddress,
              gstBreakdown = rideGstBreakdown,
              lineItems =
                let clubVatInclusive = maybe False (.driverInvoiceLineItemsVatInclusive) transporterConfig.invoiceConfig
                    inclusiveCancellation = baseCancellation + gstOnCancellation
                 in if clubVatInclusive
                      then
                        catMaybes
                          [ if inclusiveCancellation > 0
                              then Just InvoiceLineItem {description = "Cancellation Fee (Incl. VAT)", descriptionType = Just CancellationFeeInclVat, quantity = 1, unitPrice = inclusiveCancellation, lineTotal = inclusiveCancellation, isExternalCharge = False, groupId = Just "g-cancel", itemType = Just Fare}
                              else Nothing
                          ]
                      else
                        catMaybes
                          [ if baseCancellation > 0
                              then Just InvoiceLineItem {description = "Customer Cancellation Fee", descriptionType = Just CustomerCancellationFee, quantity = 1, unitPrice = baseCancellation, lineTotal = baseCancellation, isExternalCharge = False, groupId = Just "g-cancel", itemType = Just Fare}
                              else Nothing,
                            if gstOnCancellation > 0
                              then
                                Just
                                  InvoiceLineItem
                                    { description = if cancelIsVat then "Cancellation Fee VAT" else "GST on Cancellation Fee",
                                      descriptionType = Just (if cancelIsVat then CancellationFeeVat else GstOnCancellationFee),
                                      quantity = 1,
                                      unitPrice = gstOnCancellation,
                                      lineTotal = gstOnCancellation,
                                      isExternalCharge = False,
                                      groupId = Just "g-cancel",
                                      itemType = Just Tax
                                    }
                              else Nothing
                          ],
              referenceId = Nothing,
              isVat = cancelIsVat,
              issuedToTaxNo = Nothing,
              issuedByTaxNo = Nothing,
              paymentMode = Just (if isOnline then "ONLINE" else "CASH"),
              periodStart = Nothing,
              periodEnd = Nothing
            }
      case result of
        Left err -> logInfo $ "Failed to create cancellation ledger entries: " <> show err
        Right _ -> pure ()
      logInfo $ "Created customer cancellation ledger entries for bookingId: " <> booking.id.getId <> " base=" <> show baseCancellation <> " gst=" <> show gstOnCancellation <> " tds=" <> show mbTdsAmount

buildCancellationFinanceCtx ::
  ( EsqDBFlow m r,
    CacheFlow m r,
    EncFlow m r
  ) =>
  SRB.Booking ->
  DRide.Ride ->
  DTC.TransporterConfig ->
  m FinanceCtx
buildCancellationFinanceCtx booking ride transporterConfig = do
  driver <- QPerson.findById ride.driverId >>= fromMaybeM (PersonNotFound ride.driverId.getId)
  mbPanCard <- QPanCard.findByDriverId ride.driverId
  mbDriverInfo <- QDI.findById (cast ride.driverId)
  buildFinanceCtx booking ride (Just driver) mbPanCard mbDriverInfo transporterConfig True

cancellationLedgerRefs :: [Text]
cancellationLedgerRefs =
  [ walletReferenceCustomerCancellationCharges,
    walletReferenceCustomerCancellationGST,
    walletReferenceTDSDeductionCancellation,
    walletReferenceCancellationVATInput
  ]

applyCancellationLedgerAction ::
  ( EsqDBFlow m r,
    CacheFlow m r,
    EncFlow m r,
    Finance.HasActorInfo m r
  ) =>
  SRB.Booking ->
  DRide.Ride ->
  UserCancellationDues.CancellationLedgerAction ->
  DTC.TransporterConfig ->
  m ()
applyCancellationLedgerAction booking ride action transporterConfig = do
  mbCancellationDuesDetails <- QCDD.findByRideId ride.id
  let refId = booking.id.getId
      cancellationFee = fromMaybe 0 (mbCancellationDuesDetails >>= (.cancellationFee))
      cancellationFeeTax = fromMaybe 0 (mbCancellationDuesDetails >>= (.cancellationFeeTax))
      -- No overdue amounts configured => no reduction: the driver keeps the full fee.
      overdueCharge = fromMaybe cancellationFee (mbCancellationDuesDetails >>= (.overdueCancellationCharge))
      overdueTax = fromMaybe cancellationFeeTax (mbCancellationDuesDetails >>= (.overdueCancellationTax))
      -- VAT stays with the driver (OwnerLiability), GST is remitted to govt (GovtIndirect) — mirrors createDriverWalletTransaction.
      cancellationTaxDest = if fromMaybe False booking.fareParams.isVatTaxType then OwnerLiability else GovtIndirect
      -- When a cancellation goes overdue the driver only gets the (lower) overdue charge; the
      -- platform keeps the (cancellation - overdue) difference as SellerRevenue.
      overdueBenefit = max 0 (cancellationFee - overdueCharge)
      overdueBenefitTax = max 0 (cancellationFeeTax - overdueTax)
      -- Benefit tax: VAT portion is the platform's revenue, GST is remitted to govt.
      overdueBenefitTaxDest = if fromMaybe False booking.fareParams.isVatTaxType then SellerRevenue else GovtIndirect
      -- Only the driver's entries reach a payout, so only these ever carry a settlementStatus.
      overdueDriverRefs = [walletReferenceOverdueCancellationCharge, walletReferenceOverdueCancellationTax]
      overdueAllRefs = overdueDriverRefs <> [walletReferenceCancellationOverdueBenefit, walletReferenceCancellationOverdueBenefitTax]
  -- All four refs: a zero-amount entry is never written, so one ref alone can miss an overdue.
  overdueEntries <- concat <$> mapM (`getEntriesByReference` refId) overdueAllRefs
  let alreadyOverdue = not (Kernel.Prelude.null overdueEntries)
  case action of
    UserCancellationDues.SettleCancellationLedger -> do
      -- Decide once whether the settled charge is the actual cancellation fee (reversed overdue / never overdue)
      -- or the overdue charge that still stands; this single choice drives fare params + service VAT.
      let alreadyReversed = not (Kernel.Prelude.null (filter (\e -> isJust e.reversalOf) overdueEntries))
          -- Reversal covers driver + benefit entries together, so it is only safe while none of the
          -- driver's has been paid out. Benefit entries never reach a payout and must not be counted.
          reversibleEntries = filter (\e -> isNothing e.reversalOf) overdueEntries
          driverPaidOut = any (\e -> e.referenceType `elem` overdueDriverRefs && isJust e.settlementStatus) reversibleEntries
          willReverse = not (alreadyReversed || driverPaidOut || Kernel.Prelude.null reversibleEntries)
          useCancellationAmount = not alreadyOverdue || willReverse
      if alreadyOverdue
        then -- Settled after going overdue: reverse the overdue AND benefit entries, then book the actual cancellation charge.
        when willReverse $ do
          Kernel.Prelude.forM_ reversibleEntries $ \e -> void $ createReversal e.id "CancellationSettledAfterOverdue"
          let baseCancellation = cancellationFee
              gstCancellation = cancellationFeeTax
          when (baseCancellation > 0 || gstCancellation > 0) $ do
            ctx <- buildCancellationFinanceCtx booking ride transporterConfig
            result <- runFinance ctx $ do
              when (baseCancellation > 0) $ do
                transfer_ BuyerAsset BuyerExternal baseCancellation walletReferenceCustomerCancellationCharges
                transfer_ BuyerExternal OwnerLiability baseCancellation walletReferenceCustomerCancellationCharges
              when (gstCancellation > 0) $ do
                transfer_ BuyerAsset BuyerExternal gstCancellation walletReferenceCustomerCancellationGST
                transfer_ BuyerExternal cancellationTaxDest gstCancellation walletReferenceCustomerCancellationGST
            case result of
              Left err -> logError $ "Failed to book settled cancellation charge after overdue for bookingId: " <> refId <> " - " <> show err
              Right _ -> logInfo $ "Reversed overdue and booked settled cancellation charge for bookingId: " <> refId <> " base=" <> show baseCancellation <> " tax=" <> show gstCancellation
        else do
          entries <- concat <$> mapM (`getEntriesByReference` refId) cancellationLedgerRefs
          Kernel.Prelude.forM_ entries $ \e ->
            when (e.status == PENDING || e.status == DUE) $ settleEntry e.id
          logInfo $ "Settled cancellation ledger entries for bookingId: " <> refId
      settleCustomerCancellationDues booking ride
      -- Effective cancellation charge that now stands; drives both fare params and service VAT.
      -- The overdue side reads the fallback-applied amounts, not the raw columns.
      let (effectiveCancellationFee, effectiveCancellationTax) =
            if useCancellationAmount
              then (mbCancellationDuesDetails >>= (.cancellationFee), mbCancellationDuesDetails >>= (.cancellationFeeTax))
              else (overdueCharge <$ mbCancellationDuesDetails, overdueTax <$ mbCancellationDuesDetails)
      whenJust ride.fareParametersId $ QFP.updateCancellationCharges effectiveCancellationFee effectiveCancellationTax
      let cancelInclusive = fromMaybe 0 effectiveCancellationFee + fromMaybe 0 effectiveCancellationTax
          cancelServiceVatAmount = case transporterConfig.taxConfig.serviceVatPercentage of
            Just pct -> HighPrecMoney (cancelInclusive.getHighPrecMoney * (toRational pct / 100))
            Nothing -> 0
      when (cancelServiceVatAmount > 0) $ do
        ctx <- buildCancellationFinanceCtx booking ride transporterConfig
        result <- runFinance ctx $ void $ transferWithoutAttribution GovtExpense OwnerLiability cancelServiceVatAmount walletReferenceCancellationVATInput
        case result of
          Left err -> logError $ "Failed to book cancellation service VAT for bookingId: " <> refId <> " - " <> show err
          Right _ -> logInfo $ "Booked cancellation service VAT for bookingId: " <> refId <> " amount=" <> show cancelServiceVatAmount
      -- Commission on the settled cancellation fee. The PENDING guard keeps it single: a fee folded
      -- into the next ride's fare is marked PAID and commissioned at EndRide, yet can still reach
      -- this branch afterwards. Never emitted at cancel time — an unpaid fee would leave a Draft
      -- invoice in the monthly aggregate.
      -- The commission follows the fee: no overdue charge configured means the driver keeps the full
      -- fee, so the full commission applies. Hence the guard reads the charge, not the commission.
      let effectiveCancellationCommission
            | useCancellationAmount = mbCancellationDuesDetails >>= (.cancellationCommission)
            | isNothing (mbCancellationDuesDetails >>= (.overdueCancellationCharge)) =
              mbCancellationDuesDetails >>= (.cancellationCommission)
            | otherwise = mbCancellationDuesDetails >>= (.overdueCancellationCommission)
          cddWasPending = maybe False (\cdd -> cdd.paymentStatus == DCDD.PENDING) mbCancellationDuesDetails
          cancellationCommissionGross = fromMaybe 0 effectiveCancellationCommission
      when (cddWasPending && cancellationCommissionGross > 0) $ do
        let (ccBase, ccVat) = splitGrossByVatPct transporterConfig.taxConfig.commissionVatPercentage cancellationCommissionGross
        driver <- QPerson.findById ride.driverId >>= fromMaybeM (PersonNotFound ride.driverId.getId)
        ctx <- buildCancellationFinanceCtx booking ride transporterConfig
        commissionResult <- runFinance ctx $ do
          void $ transfer OwnerLiability SellerRevenue ccBase walletReferenceCancellationCommission
          when (ccVat > 0) $
            void $ transfer OwnerLiability SellerRevenue ccVat walletReferenceCancellationCommissionVAT
          invoice
            InvoiceConfig
              { invoiceType = Commission,
                issuedToType = if isJust ride.fleetOwnerId then FLEET_OWNER else DRIVER,
                issuedToId = maybe ride.driverId.getId (.getId) ride.fleetOwnerId,
                issuedToName = Just driver.firstName,
                issuedToAddress = Nothing,
                referenceId = Just booking.id.getId,
                lineItems =
                  catMaybes
                    [ Just InvoiceLineItem {description = "Cancellation Commission", descriptionType = Just CancellationCommission, quantity = 1, unitPrice = ccBase, lineTotal = ccBase, isExternalCharge = False, groupId = Just "g-commission-cancellation", itemType = Just Fare},
                      if ccVat > 0
                        then Just InvoiceLineItem {description = "Cancellation Commission VAT", descriptionType = Just CancellationCommissionTax, quantity = 1, unitPrice = ccVat, lineTotal = ccVat, isExternalCharge = False, groupId = Just "g-commission-cancellation", itemType = Just Tax}
                        else Nothing
                    ],
                gstBreakdown = Nothing,
                isVat = fromMaybe False booking.fareParams.isVatTaxType,
                issuedToTaxNo = Nothing,
                issuedByTaxNo = Nothing,
                paymentMode = Nothing,
                periodStart = Nothing,
                periodEnd = Nothing
              }
        case commissionResult of
          Left err -> logError $ "Failed to book cancellation commission for bookingId: " <> refId <> " - " <> show err
          Right _ -> logInfo $ "Booked cancellation commission for bookingId: " <> refId <> " gross=" <> show cancellationCommissionGross
    UserCancellationDues.OverdueCancellationLedger ->
      unless alreadyOverdue $ do
        entries <- concat <$> mapM (`getEntriesByReference` refId) cancellationLedgerRefs
        Kernel.Prelude.forM_ entries $ \e ->
          when (e.status == PENDING || e.status == DUE) $ voidEntry e.id "CancellationOverdue"
        when (overdueCharge > 0 || overdueTax > 0 || overdueBenefit > 0 || overdueBenefitTax > 0) $ do
          ctx <- buildCancellationFinanceCtx booking ride transporterConfig
          result <- runFinance ctx $ do
            when (overdueCharge > 0) $ do
              transfer_ BuyerAsset BuyerExternal overdueCharge walletReferenceOverdueCancellationCharge
              transfer_ BuyerExternal OwnerLiability overdueCharge walletReferenceOverdueCancellationCharge
            when (overdueTax > 0) $ do
              transfer_ BuyerAsset BuyerExternal overdueTax walletReferenceOverdueCancellationTax
              transfer_ BuyerExternal cancellationTaxDest overdueTax walletReferenceOverdueCancellationTax
            -- Platform keeps (cancellation - overdue) as SellerRevenue; funded by the customer (BuyerExternal nets to 0).
            when (overdueBenefit > 0) $ do
              transfer_ BuyerAsset BuyerExternal overdueBenefit walletReferenceCancellationOverdueBenefit
              transfer_ BuyerExternal SellerRevenue overdueBenefit walletReferenceCancellationOverdueBenefit
            when (overdueBenefitTax > 0) $ do
              transfer_ BuyerAsset BuyerExternal overdueBenefitTax walletReferenceCancellationOverdueBenefitTax
              transfer_ BuyerExternal overdueBenefitTaxDest overdueBenefitTax walletReferenceCancellationOverdueBenefitTax
          case result of
            Left err -> logError $ "Failed to create overdue cancellation ledger entries: " <> show err
            Right _ -> logInfo $ "Created overdue cancellation ledger entries for bookingId: " <> refId <> " charge=" <> show overdueCharge <> " tax=" <> show overdueTax <> " benefit=" <> show overdueBenefit <> " benefitTax=" <> show overdueBenefitTax

settleCustomerCancellationDues ::
  ( EsqDBFlow m r,
    CacheFlow m r
  ) =>
  SRB.Booking ->
  DRide.Ride ->
  m ()
settleCustomerCancellationDues booking ride =
  case booking.riderId of
    Nothing -> logError $ "settleCustomerCancellationDues: no riderId in booking " <> booking.id.getId
    Just rid -> do
      mbCancellationDuesDetails <- QCDD.findByRideId ride.id
      case mbCancellationDuesDetails of
        Just cancellationDuesDetails | cancellationDuesDetails.paymentStatus == DCDD.PENDING -> do
          riderDetails <- QRiderDetails.findById rid >>= fromMaybeM (RiderDetailsNotFound rid.getId)
          QRiderDetails.updateCancellationDues (max 0 (riderDetails.cancellationDues - cancellationDuesDetails.cancellationAmount)) rid
          QRiderDetails.updateCancellationDuesPaymentInfo cancellationDuesDetails.cancellationAmount riderDetails
          QCDD.updatePaymentStatusByRideId DCDD.PAID ride.id
          logInfo $ "Cleared customer cancellation dues for rideId: " <> ride.id.getId <> " amount=" <> show cancellationDuesDetails.cancellationAmount
        _ -> logInfo $ "No pending cancellation dues to settle for rideId: " <> ride.id.getId

buildPenaltyCheckContext ::
  ( MonadFlow m,
    EsqDBFlow m r,
    CacheFlow m r,
    Esq.EsqDBReplicaFlow m r,
    HasShortDurationRetryCfg r c,
    EncFlow m r,
    LT.HasLocationService m r,
    HasKafkaProducer r
  ) =>
  SRB.Booking ->
  DRide.Ride ->
  LatLong ->
  m TY.PenaltyCheckTagData
buildPenaltyCheckContext booking ride point = do
  now <- getCurrentTime
  numberOfCallAttempts <- QCallStatus.countCallsByEntityId ride.id

  driverDistanceFromPickupNow <- do
    distRes <- withTryCatch "driverDistanceToPickup:buildPenaltyCheckContext" $ driverDistanceToPickup booking (getCoordinates point) (getCoordinates booking.fromLocation)
    either
      ( \err -> do
          logError ("PenaltyCheckError: Failed to calculate Driver Distance to Pickup with error : " <> show err)
          return Nothing
      )
      (\d -> return $ Just d)
      distRes

  let driverDistanceFromPickupAtAcceptance = booking.distanceToPickup

  let currentTime = floor $ utcTimeToPOSIXSeconds now
      rideCreatedTime = floor $ utcTimeToPOSIXSeconds ride.createdAt
      driverArrivedAtPickup = isJust ride.driverArrivalTime

  return $
    TY.PenaltyCheckTagData
      { ride = ride,
        booking = booking,
        currentTime,
        rideCreatedTime,
        driverArrivedAtPickup,
        driverDistanceFromPickupNow,
        driverDistanceFromPickupAtAcceptance,
        numberOfCallAttempts
      }
