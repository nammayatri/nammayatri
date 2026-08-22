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
    -- re-exported from SharedLogic.CancellationOrchestrator for existing callers
    driverDistanceToPickup,
    buildCancellationContext,
    getCancellationCharges,
    customerCancellationChargesCalculation,
    CancellationChargesOutcome (..),
    getDistanceToPickup,
  )
where

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
import "beckn-spec" Domain.Types.Invoice (InvoiceType (..), IssuedToType (..))
import qualified Domain.Types.Merchant as DMerc
import qualified Domain.Types.MerchantPaymentMethod as DMPM
import qualified Domain.Types.Person as SP
import qualified Domain.Types.Ride as DRide
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
import Lib.ConfigPilot.Interface.Types (getOneConfig)
import qualified Lib.DriverCoins.Types as DCT
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
import qualified SharedLogic.CancellationDues as SCD
import qualified SharedLogic.CancellationFault as CancellationFault
import SharedLogic.CancellationOrchestrator
import qualified SharedLogic.CancellationSignals as CancellationSignals
import qualified SharedLogic.External.LocationTrackingService.Flow as LF
import qualified SharedLogic.External.LocationTrackingService.Types as LT
import SharedLogic.Finance.Wallet
import SharedLogic.GoogleTranslate (TranslateFlow)
import qualified SharedLogic.MetricsLabels as SML
import SharedLogic.Ride (releaseLien, updateOnRideStatusWithAdvancedRideCheck)
import SharedLogic.RuleBasedTierUpgrade
import qualified SharedLogic.SpecialZoneDriverDemand as SpecialZoneDriverDemand
import qualified Storage.CachedQueries.Driver.GoHomeRequest as CQDGR
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.CachedQueries.Merchant.MerchantPaymentMethod as CQMPM
import qualified Storage.CachedQueries.ValueAddNP as CQVAN
import Storage.ConfigPilot.Config.TransporterConfig (TransporterConfigDimensions (..))
import qualified Storage.Queries.Booking as QRB
import qualified Storage.Queries.BookingCancellationReason as QBCR
import qualified Storage.Queries.CancellationDuesDetails as QCDD
import qualified Storage.Queries.DriverInformation as QDI
import qualified Storage.Queries.DriverPanCard as QPanCard
import qualified Storage.Queries.DriverStats as QDriverStats
import qualified Storage.Queries.FareParameters as QFP
import qualified Storage.Queries.FleetOwnerInformation as QFOI
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.Ride as QRide
import qualified Storage.Queries.RideDetails as QRideDetails
import qualified Storage.Queries.Vehicle as QVeh
import Tools.Constants
import Tools.Error
import Tools.Event
import qualified Tools.Metrics as Metrics
import qualified Tools.Notifications as Notify
import TransactionLogs.Types

-- main fn
cancelRideImpl ::
  ( EncFlow m r,
    EsqDBReplicaFlow m r,
    CacheFlow m r,
    EsqDBFlow m r,
    Metrics.HasBPPMetrics m r,
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
    HasFlowEnv m r '["fabricGatewayBaseUrl" ::: BaseUrl],
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
            transporterConfig <- getOneConfig (TransporterConfigDimensions {merchantOperatingCityId = booking.merchantOperatingCityId.getId}) Nothing >>= fromMaybeM (TransporterConfigNotFound booking.merchantOperatingCityId.getId)
            (cancellationDisToPickup, _mbDriverLocation) <- getDistanceToPickup booking (Just ride)
            -- ONE decision (signals → fault verdict → consequence-matrix row) feeds every
            -- consequence below through the orchestrator; nothing re-derives it.
            decision <- decideCancellationConsequences booking ride transporterConfig DCT.CancellationByDriver bookingCReason.reasonCode cancellationDisToPickup
            let consequenceCtx = ConsequenceCtx {merchant = merchant, booking = booking, ride = ride, transporterConfig = transporterConfig, source = bookingCReason.source, decision = decision}
            void $ updateNammaTagsForCancelledRide booking ride bookingCReason transporterConfig decision.faultVerdict
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
            cancelRideTransaction booking ride bookingCReason merchant rideEndedBy transporterConfig driver
            -- Matrix-row-driven consequences (SharedLogic.CancellationOrchestrator):
            -- blacklist, driver overlay, coin event, driver money, rate counting — then
            -- the customer-side charge (dues + counters + ledger entries).
            applyImmediateConsequences consequenceCtx doCancellationRateBasedBlocking
            chargesOutcome <- applyTerminalConsequences consequenceCtx (\base gst -> createCancellationLedgerEntries booking ride base gst transporterConfig)
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

            fork "cancelRide - Notify driver" $ do
              triggerRideCancelledEvent RideEventData {ride = ride{status = DRide.CANCELLED}, personId = driver.id, merchantId = merchantId}
              triggerBookingCancelledEvent BookingEventData {booking = booking{status = SRB.CANCELLED}, personId = driver.id, merchantId = merchantId}
              Notify.notifyOnCancel ride.merchantOperatingCityId ride.id booking driver bookingCReason.source
            fork "cancelRide/ReAllocate - Notify BAP" $ do
              isReallocated <- reAllocateBookingIfPossible isValueAddNP False merchant booking ride driver vehicle bookingCReason isForceReallocation
              unless isReallocated $ do
                -- Reload ride to get persisted cancellationFee/cancellationFeeTax
                updatedRide <- QRide.findById ride.id
                BP.sendBookingCancelledUpdateToBAP booking merchant bookingCReason.source (chargesOutcome >>= (.fee)) (chargesOutcome >>= (.tax)) updatedRide
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
    Metrics.HasBPPMetrics m r,
    Finance.HasActorInfo m r
  ) =>
  SRB.Booking ->
  DRide.Ride ->
  SBCR.BookingCancellationReason ->
  DMerc.Merchant ->
  DRide.RideEndedBy ->
  DTC.TransporterConfig ->
  SP.Person ->
  m ()
cancelRideTransaction booking ride bookingCReason merchant rideEndedBy _transporterConfig _driver = do
  let driverId = cast ride.driverId
      isPrepaidSubscriptionAndWalletEnabled = fromMaybe False merchant.prepaidSubscriptionAndWalletEnabled
  when isPrepaidSubscriptionAndWalletEnabled $ releaseLien booking ride
  void $ CQDGR.setDriverGoHomeIsOnRideStatus ride.driverId booking.merchantOperatingCityId False
  updateOnRideStatusWithAdvancedRideCheck driverId (Just ride)
  when booking.isScheduled $ QDI.updateLatestScheduledBookingAndPickup Nothing Nothing driverId
  void $ LF.rideDetails ride.id DRide.CANCELLED merchant.id ride.driverId booking.fromLocation.lat booking.fromLocation.lon Nothing (Just $ (LT.Car $ LT.CarRideInfo {pickupLocation = LatLong (booking.fromLocation.lat) (booking.fromLocation.lon), minDistanceBetweenTwoPoints = Nothing, rideStops = Just $ map (\stop -> LatLong stop.lat stop.lon) booking.stops}))
  void $ QRide.updateStatusAndRideEndedBy ride.id DRide.CANCELLED rideEndedBy
  QBCR.upsert bookingCReason
  cityLabel <- SML.getCityLabel booking.merchantOperatingCityId
  Metrics.incrementRideCancelledCount merchant.shortId.getShortId cityLabel (show booking.vehicleServiceTier) (show bookingCReason.source) (SML.distanceBucketLabel (SML.distanceBucketEdges transporterConfig) booking.estimatedDistance)
  void $ QRB.updateStatus booking.id SRB.CANCELLED
  when (bookingCReason.source == SBCR.ByDriver) $ QDriverStats.updateIdleTime driverId

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
  Maybe CancellationFault.FaultVerdict ->
  m [LYT.TagNameValue]
updateNammaTagsForCancelledRide booking ride bookingCReason transporterConfig mbFaultVerdict = do
  now <- getCurrentTime
  callAtemptByDriver <- CancellationSignals.getCallAttemptByDriver ride.id
  let currentTime = floor $ utcTimeToPOSIXSeconds now
      rideCreatedTime = floor $ utcTimeToPOSIXSeconds ride.createdAt
      driverArrivalTime = floor . utcTimeToPOSIXSeconds <$> (ride.driverArrivalTime)
      bookingCreatedTime = floor $ utcTimeToPOSIXSeconds booking.createdAt
      tagData =
        TY.CancelRideTagData
          { ride = ride{status = DRide.CANCELLED},
            booking = booking{status = SRB.CANCELLED},
            cancellationReason = bookingCReason,
            merchantOperatingCityId = booking.merchantOperatingCityId,
            faultVerdict = (\v -> show v.atFault) <$> mbFaultVerdict,
            faultRule = (.rule) <$> mbFaultVerdict,
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
      merchantOperatingCity <- CQMOC.findById booking.merchantOperatingCityId >>= fromMaybeM (MerchantOperatingCityDoesNotExist booking.merchantOperatingCityId.getId)
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
              gstBreakdown =
                computeGstBreakdownByPlace
                  rideGst
                  (Just $ show merchantOperatingCity.state)
                  booking.fromLocation.address.state
                  (Just $ show merchantOperatingCity.city)
                  booking.fromLocation.address.city
                  gstOnCancellation,
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
              referenceId = Just booking.id.getId,
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
  SCD.CancellationLedgerAction ->
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
    SCD.SettleCancellationLedger -> do
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
      SCD.settleCustomerCancellationDues booking ride
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
          void $ transfer OwnerLiability SellerRevenue ccBase walletReferenceCancellationCommission Nothing
          when (ccVat > 0) $
            void $ transfer OwnerLiability SellerRevenue ccVat walletReferenceCancellationCommissionVAT Nothing
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
    SCD.OverdueCancellationLedger ->
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

-- buildPenaltyCheckContext was removed: the driver penalty preview (postPenaltyCheck)
-- now resolves the consequence matrix directly instead of simulating PenaltyCheck tags.
