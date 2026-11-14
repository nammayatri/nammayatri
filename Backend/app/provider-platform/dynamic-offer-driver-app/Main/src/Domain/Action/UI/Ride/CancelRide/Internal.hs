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
    settleCancellationLedger,
    markCancellationLedgerDue,
    -- re-exported from SharedLogic.CancellationOrchestrator for existing callers
    driverDistanceToPickup,
    buildCancellationContext,
    getCancellationCharges,
    CancellationChargesOutcome (..),
    getDistanceToPickup,
  )
where

import BecknV2.OnDemand.FareBreakup.Tax (TaxBreakup)
import qualified BecknV2.OnDemand.FareBreakup.Tax as Tax
import qualified Control.Monad.Catch as C
import qualified Data.HashMap.Strict as HM
import qualified Data.HashMap.Strict as HMS
import qualified Data.Map as M
-- import qualified Lib.Yudhishthira.Event as Yudhishthira

-- import qualified Lib.Yudhishthira.Tools.Utils as LYTU

import Data.Time.Clock (utctDay)
import qualified Domain.Types.Booking as SRB
import qualified Domain.Types.BookingCancellationReason as SBCR
import qualified Domain.Types.CancellationDuesDetails as DCDD
import Domain.Types.FinancialYear (financialYearOf)
import "beckn-spec" Domain.Types.Invoice (InvoiceType (..), IssuedToType (..))
import qualified Domain.Types.Merchant as DMerc
import qualified Domain.Types.MerchantPaymentMethod as DMPM
import Domain.Types.PaymentChargeBearer (PaymentChargeBearer (..))
import qualified Domain.Types.Person as SP
import qualified Domain.Types.Ride as DRide
import qualified Domain.Types.TransporterConfig as DTC
import qualified Domain.Types.VehicleVariant as Veh
import EulerHS.Prelude hiding (whenJust)
import Kernel.External.Maps
import Kernel.External.Types (ServiceFlow)
import Kernel.Prelude hiding (any, elem, map, mapM_, notElem)
import Kernel.Storage.Clickhouse.Config
import qualified Kernel.Storage.Clickhouse.Config as CH
import qualified Kernel.Storage.ClickhouseV2 as CHV2
import qualified Kernel.Storage.Esqueleto as Esq hiding (whenJust_)
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Streaming.Kafka.Producer.Types (HasKafkaProducer, KafkaProducerTools)
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics, DeploymentVersion)
import Kernel.Types.Id
import Kernel.Types.Version (CloudType)
import Kernel.Utils.Common
import Lib.ConfigPilot.Interface.Types (getOneConfig)
import Lib.Finance (AccountRole (..), EntryStatus (..), FinanceCtx, FinanceM, InvoiceConfig (..), InvoiceLineItem (..), ItemType (..), LineItemDescription (..), getEntriesByReference, includeEntryIds, invoice, settleEntry, transferPending, updateEntryStatus, voidEntry, withOwnerLock)
import qualified Lib.Finance.Core.Types as Finance
import Lib.Finance.Storage.Beam.BeamFlow (BeamFlow)
import qualified Lib.Finance.Storage.Queries.Invoice as QFInvoice
import qualified "location-updates" Lib.LocationUpdates as LocUpd
import Lib.Scheduler (SchedulerType)
import Lib.Scheduler.JobStorageType.SchedulerType (createJobIn)
import Lib.SessionizerMetrics.Types.Event
import SharedLogic.Allocator (AllocatorJobType (..), GenerateInvoicePdfJobData (..))
import qualified SharedLogic.Analytics as Analytics
import qualified SharedLogic.BehaviourManagement.PickupStallState as PickupStallState
import qualified SharedLogic.CallBAP as BP
import SharedLogic.CallBAPInternal
import SharedLogic.Cancel
import qualified SharedLogic.CancellationDues as SCD
import SharedLogic.CancellationOrchestrator
import qualified SharedLogic.DriverFyEarnings as SDFE
import qualified SharedLogic.DriverSupplyCounter as DSC
import qualified SharedLogic.External.LocationTrackingService.Flow as LF
import qualified SharedLogic.External.LocationTrackingService.Types as LT
import qualified SharedLogic.Finance.B2CQRCode as B2CQRCode
import SharedLogic.Finance.GstBreakdown
import SharedLogic.Finance.PostActions (runFinance)
import qualified SharedLogic.Finance.SubscriptionConsumption as SubscriptionConsumption
import qualified SharedLogic.Finance.TdsRate as STds
import SharedLogic.Finance.Wallet
import qualified SharedLogic.FleetEngine as FleetEngine
import SharedLogic.GoogleTranslate (TranslateFlow)
import qualified SharedLogic.MetricsLabels as SML
import SharedLogic.Ride (releaseLien, updateOnRideStatusWithAdvancedRideCheck)
import SharedLogic.RuleBasedTierUpgrade
import qualified SharedLogic.ScheduledBooking.OverlapCheck as SBOC
import qualified SharedLogic.SearchTryLocker as CS
import qualified SharedLogic.SpecialZoneDriverDemand as SpecialZoneDriverDemand
import qualified Storage.CachedQueries.Driver.GoHomeRequest as CQDGR
import qualified Storage.CachedQueries.Merchant as CQM
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
    Metrics.HasDriverSearchRequestResponseMetrics m r,
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
    HasField "serviceClickhouseCfg" r CH.ClickhouseCfg,
    HasField "serviceClickhouseEnv" r CH.ClickhouseEnv,
    CHV2.HasClickhouseEnv CHV2.APP_SERVICE_CLICKHOUSE m,
    HasField "blackListedJobs" r [Text],
    HasField "enableLtsPoolDataForPooling" r Bool,
    Redis.HedisLTSFlowEnv r,
    ClickhouseFlow m r,
    Finance.HasActorInfo m r,
    BeamFlow m r,
    CoreMetrics m,
    HasField "driverQuoteExpirationSeconds" r NominalDiffTime,
    HasFlowEnv m r '["version" ::: DeploymentVersion],
    HasPrettyLogger m r,
    ServiceFlow m r,
    HasField "quoteRespondCoolDown" r Int,
    HasField "driverUnlockDelay" r Seconds,
    C.MonadCatch m,
    Analytics.PublishesFleetAnalytics m
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
            fork "FleetEngine: cancel trip" $ FleetEngine.notifyTripCancelled booking.merchantOperatingCityId ride.id
            isValueAddNP <- CQVAN.isValueAddNP booking.bapId
            let merchantId = booking.providerId
            merchant <-
              CQM.findById merchantId
                >>= fromMaybeM (MerchantNotFound merchantId.getId)
            transporterConfig <- getOneConfig (TransporterConfigDimensions {merchantOperatingCityId = booking.merchantOperatingCityId.getId}) Nothing >>= fromMaybeM (TransporterConfigNotFound booking.merchantOperatingCityId.getId)
            unless (isValidRide ride) $ throwError (InternalError "Ride is not valid for cancellation")
            (cancellationDisToPickup, _mbDriverLocation) <- getDistanceToPickup booking (Just ride)
            -- ONE decision (signals → fault verdict → consequence-matrix row) feeds every
            -- consequence below through the orchestrator; nothing re-derives it.
            decision <- decideCancellationConsequences booking ride transporterConfig bookingCReason.source bookingCReason.reasonCode cancellationDisToPickup
            driver <- QPerson.findById ride.driverId >>= fromMaybeM (PersonNotFound ride.driverId.getId)
            let consequenceCtx = ConsequenceCtx {merchant = merchant, booking = booking, ride = ride, transporterConfig = transporterConfig, driver = driver, source = bookingCReason.source, decision = decision}
            mbVehicle <- QVeh.findById ride.driverId
            vehicle <- case mbVehicle of
              Just v -> pure v
              Nothing
                | allowSnapshotVehicleFallback -> do
                  logWarning $ "Vehicle missing for driver " <> ride.driverId.getId <> " on cancelled ride " <> ride.id.getId <> "; using ride_details snapshot (ops cancel)"
                  rideDetails <- QRideDetails.findById ride.id >>= fromMaybeM (RideNotFound ride.id.getId)
                  pure $ BP.buildVehicleFromRideDetailsSnapshot booking ride rideDetails
                | otherwise -> throwError (DriverWithoutVehicle ride.driverId.getId)
            cancelRideTransaction booking ride bookingCReason merchant rideEndedBy transporterConfig driver
            -- the decision above already consumed the live pickup journey; persist it on the ride
            fork "flush pickup journey on cancel" $ PickupStallState.flushPickupJourney ride Nothing
            -- Matrix-row-driven consequences (SharedLogic.CancellationOrchestrator):
            -- blacklist, driver overlay, coin event, driver money, rate counting — then
            -- the customer-side charge (dues + counters + ledger entries).
            applyImmediateConsequences consequenceCtx doCancellationRateBasedBlocking
            chargesOutcome <- applyTerminalConsequences consequenceCtx (\base feeTax mbCreditDebit -> createCancellationLedgerEntries booking ride base feeTax transporterConfig mbCreditDebit)
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
              -- INSTANT_AUTO bookings reassign through the same broadcast path as every other tier.
              isReallocated <- reAllocateBookingIfPossible isValueAddNP False merchant booking ride driver vehicle bookingCReason isForceReallocation
              unless isReallocated $ do
                -- Reload ride to get persisted cancellationFee/cancellationFeeTax
                updatedRide <- QRide.findById ride.id
                BP.sendBookingCancelledUpdateToBAP booking merchant bookingCReason.source (chargesOutcome >>= (.fee)) ((.total) <$> (chargesOutcome >>= (.feeTax))) updatedRide
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
cancelRideTransaction booking ride bookingCReason merchant rideEndedBy transporterConfig _driver = do
  let driverId = cast ride.driverId
  -- releaseLien gates itself on prepaid-or-wallet being enabled.
  releaseLien booking ride
  void $ CQDGR.setDriverGoHomeIsOnRideStatus ride.driverId booking.merchantOperatingCityId False
  updateOnRideStatusWithAdvancedRideCheck driverId (Just ride)
  when booking.isScheduled $
    -- recompute the gate under the per-driver hold lock to avoid racing an accept/release
    CS.withDriverScheduledHoldLock driverId $ do
      mbNextHold <- SBOC.nextScheduledHoldAfterRelease transporterConfig driverId booking.id
      QDI.updateLatestScheduledBookingAndPickup (fst <$> mbNextHold) (snd <$> mbNextHold) driverId
  void $ LF.rideDetails ride.id DRide.CANCELLED merchant.id ride.driverId booking.fromLocation.lat booking.fromLocation.lon Nothing (Just $ (LT.Car $ LT.CarRideInfo {pickupLocation = LatLong (booking.fromLocation.lat) (booking.fromLocation.lon), minDistanceBetweenTwoPoints = Nothing, rideStops = Just $ map (\stop -> LatLong stop.lat stop.lon) booking.stops, dropLocation = Nothing}))
  void $ QRide.updateStatusAndRideEndedBy ride.id DRide.CANCELLED rideEndedBy
  QBCR.upsert bookingCReason
  -- Only a ride that started was ever counted; `ride` here is the pre-update snapshot,
  -- so its status still reflects whether the trip was in progress.
  when (ride.status == DRide.INPROGRESS) $ DSC.recordOnRideChange booking.merchantOperatingCityId False
  cityLabel <- SML.getCityLabel booking.merchantOperatingCityId
  let (pickupZone, dropZone) = SML.specialZoneLabels booking.area
  Metrics.incrementRideCancelledCount merchant.shortId.getShortId cityLabel (show booking.vehicleServiceTier) (show bookingCReason.source) (SML.distanceBucketLabel (SML.distanceBucketEdges transporterConfig) booking.estimatedDistance) pickupZone dropZone
  void $ QRB.updateStatus booking.id SRB.CANCELLED
  -- This ride's pickup leg will never be finalized, so nothing else clears its buffers.
  -- Leaving them would make the driver's next assignment measure its pickup distance from
  -- wherever this one left off.
  LocUpd.resetPickupLegState ride.driverId
  when (bookingCReason.source == SBCR.ByDriver) $ QDriverStats.updateIdleTime driverId

-- | Create BPP-side finance ledger entries + invoice for a customer cancellation charge.
-- Extracted so it can be called from both cancelRideTransaction (driver-cancel path)
-- and Domain.Action.Beckn.Cancel (rider-cancel via Beckn path).
createCancellationLedgerEntries ::
  ( EsqDBFlow m r,
    CacheFlow m r,
    EncFlow m r,
    BeamFlow m r,
    Finance.HasActorInfo m r,
    Redis.HedisFlow m r,
    Redis.HedisLTSFlowEnv r,
    HasField "serviceClickhouseCfg" r CH.ClickhouseCfg,
    HasField "serviceClickhouseEnv" r CH.ClickhouseEnv,
    HasField "maxShards" r Int,
    HasField "schedulerSetName" r Text,
    HasField "schedulerType" r SchedulerType,
    HasField "jobInfoMap" r (M.Map Text Bool),
    HasField "blackListedJobs" r [Text]
  ) =>
  SRB.Booking ->
  DRide.Ride ->
  HighPrecMoney ->
  Maybe TaxBreakup ->
  DTC.TransporterConfig ->
  Maybe HighPrecMoney ->
  m ()
createCancellationLedgerEntries booking ride baseCancellation cancellationFeeTax transporterConfig mbRideCreditDebit = do
  let riderId = booking.riderId
      taxOnCancellation = Tax.taxTotal cancellationFeeTax
  case riderId of
    Nothing -> logError "createCancellationLedgerEntries: riderId not present in booking"
    Just rid -> do
      let driverOrFleetPersonId = fromMaybe ride.driverId ride.fleetOwnerId
      -- One wallet's postings must not interleave (balance update is read-modify-write).
      withOwnerLock driverOrFleetPersonId.getId $ do
        mbPanCard <- QPanCard.findByDriverId driverOrFleetPersonId
        driver <- QPerson.findById ride.driverId >>= fromMaybeM (PersonNotFound ride.driverId.getId)
        mbDriverInfo <- QDI.findById (cast ride.driverId)
        -- Resolve the materialized tds_rate for the tax subject (fleet owner if
        -- it's a fleet ride, else the driver), materialising it from the cohort if
        -- the column is empty -- same contract as ride end, so a cancellation and a
        -- completed ride can never price off different rates for the same person.
        (mbFleetInfo, mbStoredTdsRate) <- case ride.fleetOwnerId of
          Just fleetOwnerId -> do
            mbFleetInfo' <- QFOI.findByPrimaryKey (cast fleetOwnerId)
            rate <- STds.ensureTdsRateFor transporterConfig fleetOwnerId True (mbFleetInfo' >>= (.tdsRate))
            pure (mbFleetInfo', rate)
          Nothing -> do
            rate <- STds.ensureTdsRateFor transporterConfig ride.driverId False (mbDriverInfo >>= (.tdsRate))
            pure (Nothing, rate)
        let rideGst = transporterConfig.taxConfig.rideGst
            cancelIsVat = fromMaybe False booking.fareParams.isVatTaxType
            -- VAT stays with the driver (OwnerLiability), GST is remitted to govt (GovtIndirect) — mirrors createDriverWalletTransaction.
            cancellationTaxDest = if cancelIsVat then OwnerLiability else GovtIndirect
            cancellationComponents =
              [ (baseCancellation, walletReferenceCustomerCancellationCharges, OwnerLiability),
                (taxOnCancellation, walletReferenceCustomerCancellationTax (if cancelIsVat then Tax.VAT else Tax.GST), cancellationTaxDest)
              ]
            mbTdsRate =
              if panAadhaarLinkTdsEnabled transporterConfig.taxConfig
                then computeEffectiveTdsRate mbPanCard mbStoredTdsRate transporterConfig.taxConfig
                else (.rate) <$> transporterConfig.taxConfig.defaultTdsRate
            fyStartMonth = transporterConfig.analyticsConfig.financialYearStartMonth
        -- Cancellation charges the driver / FO receives are earnings and are subject
        -- to TDS, treated like ride fare. baseCancellation is already net of GST
        -- (gstOnCancellation is separate), so it is the TDS base as-is.
        cancelLocalDate <- utctDay <$> getLocalCurrentTime transporterConfig.timeDiffFromUtc
        mbTdsAmount <-
          Redis.withWaitAndLockRedis (SDFE.makeFyEarningsLockKey driverOrFleetPersonId.getId) 10 1000 $ do
            mbPersistedFareParams <- QFP.findById booking.fareParams.id
            case mbPersistedFareParams of
              Just fp | isJust fp.tdsProcessedAt -> pure fp.tdsAmount
              _ -> do
                fyToDate <- SDFE.getFyToDateNetEarnings driverOrFleetPersonId (financialYearOf fyStartMonth cancelLocalDate)
                let mbAmount = do
                      rate <- mbTdsRate
                      let rawAmount = baseCancellation * realToFrac rate
                          gatedAmount = applyThresholdBenefit transporterConfig.taxConfig (Just fyToDate) mbPanCard baseCancellation rawAmount
                      if gatedAmount > 0 then Just gatedAmount else Nothing
                SDFE.addQuarterNetEarnings driverOrFleetPersonId fyStartMonth cancelLocalDate (baseCancellation - fromMaybe 0 mbAmount) (fromMaybe 0 mbAmount)
                processedAt <- getCurrentTime
                QFP.updateTdsDeduction mbAmount mbTdsRate processedAt booking.fareParams.id
                pure mbAmount
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
        ctx <- buildFinanceCtx booking ride (Just driver) mbPanCard mbDriverInfo transporterConfig True Nothing
        rideGstBreakdown <-
          computeGstBreakdownForRideOwner
            rideGst
            booking.fromLocation
            ride
            mbFleetInfo
            mbDriverInfo
            taxOnCancellation
        mbPendingDues <- mfilter ((== DCDD.PENDING) . (.paymentStatus)) <$> QCDD.findByRideId ride.id
        result <- runFinance ctx $ do
          -- Post ledger entries only when there is an actual charge. For a ₹0 valid buyer
          -- cancellation we skip the transfers and still create a standalone ₹0 invoice below
          -- (no linked entries → no GST tax transaction).
          when (baseCancellation + taxOnCancellation > 0) $ do
            mapM_
              (\(amt, ref, dest) -> void $ transferPending BuyerAsset dest amt ref)
              cancellationComponents
            whenJust mbTdsAmount $ \tdsAmount ->
              void $ transferPending OwnerLiability GovtDirect tdsAmount walletReferenceTDSDeductionCancellation
            cancellationChargeTransfers booking (mbPendingDues >>= (.cancellationCommission)) (mbPendingDues >>= (.cancellationCommissionTaxBreakup)) mbPendingDues
          invoice
            InvoiceConfig
              { invoiceType = RideCancellation,
                issuedToType = CUSTOMER,
                issuedToId = rid.getId,
                issuedToName = booking.riderName,
                issuedToAddress = booking.fromLocation.address.fullAddress,
                gstBreakdown = rideGstBreakdown,
                lineItems =
                  catMaybes
                    [ if baseCancellation > 0 || baseCancellation + taxOnCancellation == 0
                        then Just InvoiceLineItem {description = "Customer Cancellation Fee", descriptionType = Just CustomerCancellationFee, quantity = 1, unitPrice = baseCancellation, lineTotal = baseCancellation, isExternalCharge = False, groupId = Just "g-cancel", itemType = Just Fare}
                        else Nothing,
                      if taxOnCancellation > 0
                        then
                          Just
                            InvoiceLineItem
                              { description = if cancelIsVat then "Cancellation Fee VAT" else "GST on Cancellation Fee",
                                descriptionType = Just (if cancelIsVat then CancellationFeeVat else GstOnCancellationFee),
                                quantity = 1,
                                unitPrice = taxOnCancellation,
                                lineTotal = taxOnCancellation,
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
          Right (mbInvoiceId, _) ->
            whenJust mbInvoiceId $ \invId -> do
              -- GST place of supply = the pickup State.
              QFInvoice.updatePlaceOfSupply (gstPlaceOfSupply booking.fromLocation) Nothing Nothing invId
              -- B2C self-generated unsigned QR (no IRN) on the customer tax invoice.
              B2CQRCode.generateB2CQRForInvoice transporterConfig invId
              -- Link the cancellation invoice to the booking so the ONDC on_status
              -- documents[] (and the invoice-PDF read paths) can resolve + attach it.
              QRB.updateFinanceInvoiceId booking.id (Just invId.getId)
              -- Render + store the PDF off the cancel path, after the place-of-supply and
              -- QR stamps above (a stored PDF is final), so ONDC on_status can attach it.
              when (fromMaybe False (transporterConfig.invoiceConfig >>= (.enableInvoicePdfS3Storage))) $
                createJobIn @_ @'GenerateInvoicePdf (Just booking.providerId) (Just booking.merchantOperatingCityId) 0 $
                  GenerateInvoicePdfJobData {invoiceId = invId.getId}
        logInfo $ "Created customer cancellation ledger entries for bookingId: " <> booking.id.getId <> " base=" <> show baseCancellation <> " gst=" <> show taxOnCancellation <> " tds=" <> show mbTdsAmount
        whenJust mbRideCreditDebit $ \creditDebit ->
          SubscriptionConsumption.consumeCancellationRideCredit booking ride creditDebit transporterConfig

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
  buildFinanceCtx booking ride (Just driver) mbPanCard mbDriverInfo transporterConfig True Nothing

cancellationLedgerRefs :: [Text]
cancellationLedgerRefs =
  [ walletReferenceCustomerCancellationCharges,
    walletReferenceCustomerCancellationTax Tax.GST,
    walletReferenceCustomerCancellationTax Tax.VAT,
    walletReferenceTDSDeductionCancellation,
    walletReferencePaymentChargePaidByCustomer,
    walletReferencePaymentChargeTaxPaidByCustomer Tax.GST,
    walletReferencePaymentChargeTaxPaidByCustomer Tax.VAT,
    walletReferencePGPaymentCharges,
    walletReferencePGPaymentChargesTax Tax.GST,
    walletReferencePGPaymentChargesTax Tax.VAT
  ]
    <> cancellationCommissionLedgerRefs

cancellationCommissionLedgerRefs :: [Text]
cancellationCommissionLedgerRefs =
  [ walletReferenceCancellationCommission,
    walletReferenceCancellationCommissionTax Tax.GST,
    walletReferenceCancellationCommissionTax Tax.VAT
  ]

cancellationChargeTransfers ::
  forall m r.
  (BeamFlow m r, Finance.HasActorInfo m r) =>
  SRB.Booking ->
  Maybe HighPrecMoney ->
  Maybe TaxBreakup ->
  Maybe DCDD.CancellationDuesDetails ->
  FinanceM m ()
cancellationChargeTransfers booking mbCommission mbCommissionTax mbDues = do
  let taxRegime = if fromMaybe False booking.fareParams.isVatTaxType then Tax.VAT else Tax.GST
      commissionTax = Tax.taxTotal mbCommissionTax
      commissionBase = fromMaybe 0 mbCommission - commissionTax
      charge = fromMaybe 0 (mbDues >>= (.paymentCharge))
      chargeTax = Tax.taxTotal (mbDues >>= (.paymentChargeTaxBreakup))
      mbBearer = (.bearer) <$> booking.paymentChargeDetails
      postIfPositive :: AccountRole -> AccountRole -> HighPrecMoney -> Text -> FinanceM m ()
      postIfPositive from to amount ref = when (amount > 0) $ void $ transferPending from to amount ref
  postIfPositive OwnerLiability SellerRevenue commissionBase walletReferenceCancellationCommission
  postIfPositive OwnerLiability SellerRevenue commissionTax (walletReferenceCancellationCommissionTax taxRegime)
  when (mbBearer == Just PAYMENT_CUSTOMER) $ do
    postIfPositive BuyerAsset OwnerLiability charge walletReferencePaymentChargePaidByCustomer
    postIfPositive BuyerAsset OwnerLiability chargeTax (walletReferencePaymentChargeTaxPaidByCustomer taxRegime)
  when (mbBearer `elem` [Just PAYMENT_CUSTOMER, Just PAYMENT_DRIVER]) $ do
    postIfPositive OwnerLiability SellerLiability charge walletReferencePGPaymentCharges
    postIfPositive OwnerLiability SellerLiability chargeTax (walletReferencePGPaymentChargesTax taxRegime)

markCancellationLedgerDue ::
  ( EsqDBFlow m r,
    CacheFlow m r,
    Finance.HasActorInfo m r
  ) =>
  SRB.Booking ->
  m ()
markCancellationLedgerDue booking = do
  entries <- concat <$> mapM (`getEntriesByReference` booking.id.getId) cancellationLedgerRefs
  Kernel.Prelude.forM_ (filter ((== PENDING) . (.status)) entries) $ \e -> updateEntryStatus e.id DUE
  logInfo $ "Marked cancellation ledger entries due for bookingId: " <> booking.id.getId

settleCancellationLedger ::
  ( EsqDBFlow m r,
    CacheFlow m r,
    EncFlow m r,
    Finance.HasActorInfo m r,
    Redis.HedisFlow m r,
    Redis.HedisLTSFlowEnv r
  ) =>
  SRB.Booking ->
  DRide.Ride ->
  DTC.TransporterConfig ->
  m ()
settleCancellationLedger booking ride transporterConfig =
  withOwnerLock (maybe ride.driverId.getId (.getId) ride.fleetOwnerId) $ do
    mbCancellationDuesDetails <- QCDD.findByRideId ride.id
    let refId = booking.id.getId
        taxRegime = if fromMaybe False booking.fareParams.isVatTaxType then Tax.VAT else Tax.GST
        cancellationPaymentCharge = fromMaybe 0 (mbCancellationDuesDetails >>= (.paymentCharge))
        cancellationPaymentChargeTax = Tax.taxTotal (mbCancellationDuesDetails >>= (.paymentChargeTaxBreakup))
        collectedOnNextRide = maybe False (\cdd -> SCD.carriedInRiderBalance cdd && cdd.paymentStatus == DCDD.PAID) mbCancellationDuesDetails
    entries <- concat <$> mapM (`getEntriesByReference` refId) cancellationLedgerRefs
    Kernel.Prelude.forM_ entries $ \e ->
      when (e.status == PENDING || e.status == DUE) $
        if collectedOnNextRide then voidEntry e.id "CancellationDuesPostedAtRideEnd" else settleEntry e.id
    logInfo $ "Settled cancellation ledger entries for bookingId: " <> refId
    SCD.settleCustomerCancellationDues booking ride
    whenJust ride.fareParametersId $ QFP.updateCancellationCharges (mbCancellationDuesDetails >>= (.cancellationFee)) (mbCancellationDuesDetails >>= (.cancellationFeeTax))
    whenJust (mfilter ((== DCDD.PENDING) . (.paymentStatus)) mbCancellationDuesDetails) $ \_ -> do
      commissionEntries <- filter ((== SETTLED) . (.status)) . concat <$> mapM (`getEntriesByReference` refId) cancellationCommissionLedgerRefs
      let ccBase = Kernel.Prelude.sum [e.amount | e <- commissionEntries, e.referenceType == walletReferenceCancellationCommission]
          ccVat = Kernel.Prelude.sum [e.amount | e <- commissionEntries, e.referenceType /= walletReferenceCancellationCommission]
      unless (Kernel.Prelude.null commissionEntries) $ do
        driver <- QPerson.findById ride.driverId >>= fromMaybeM (PersonNotFound ride.driverId.getId)
        ctx <- buildCancellationFinanceCtx booking ride transporterConfig
        commissionResult <- runFinance ctx $ do
          includeEntryIds ((.id) <$> commissionEntries)
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
          Right _ -> logInfo $ "Settled cancellation commission for bookingId: " <> refId <> " base=" <> show ccBase <> " tax=" <> show ccVat
      when (cancellationPaymentCharge > 0 && ((.bearer) <$> booking.paymentChargeDetails) == Just PAYMENT_PLATFORM) $ do
        ctx <- buildCancellationFinanceCtx booking ride transporterConfig
        chargeResults <- mapM (\(chargeAmount, chargeRef) -> recordStripeChargeLedger ctx FundByPlatform chargeAmount chargeRef) [(cancellationPaymentCharge, walletReferencePGPaymentCharges), (cancellationPaymentChargeTax, walletReferencePGPaymentChargesTax taxRegime)]
        Kernel.Prelude.forM_ chargeResults $ either (\err -> logError $ "Failed to book platform-borne cancellation payment charge for bookingId: " <> refId <> " - " <> show err) (const (pure ()))

-- buildPenaltyCheckContext was removed: the driver penalty preview (postPenaltyCheck)
-- now resolves the consequence matrix directly instead of simulating PenaltyCheck tags.
