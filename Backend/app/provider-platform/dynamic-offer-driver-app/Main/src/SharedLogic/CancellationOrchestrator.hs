{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- | The single entry point for every consequence of a ride cancellation
-- (dev/docs/cancellation-consequence-matrix-plan.md).
--
-- Both cancel flows (customer via Domain.Action.Beckn.Cancel, driver/ops via
-- Domain.Action.UI.Ride.CancelRide.Internal) run the same three steps:
--
--   1. 'decideCancellationConsequences' — signals → fault verdict → consequence-matrix
--      row, resolved ONCE (Redis-cached per ride) into a 'CancellationDecision'.
--   2. 'applyImmediateConsequences'     — driver-side consequences: driver→rider
--      blacklist, driver overlay notification, the coin event, the driver money
--      penalty/credit, and driver cancellation-rate counting.
--   3. 'applyTerminalConsequences'      — the customer-side money consequences.
--
-- BOTH run on every cancellation with an assigned ride, regardless of whether the
-- booking reallocates afterwards: the matrix (fault verdict + row) alone decides who
-- pays what — reallocation is booking-lifecycle mechanics, not a consequence gate.
--
-- Each matrix output column has exactly ONE dedicated executor function here; an executor
-- self-gates on its own column (absent column = no-op), so adding a consequence means one
-- column + one executor — no dispatch scattered across the codebase.
module SharedLogic.CancellationOrchestrator
  ( -- * The one decision every consequence reads
    CancellationDecision (..),
    ConsequenceCtx (..),
    cancellationSourceToType,
    decideCancellationConsequences,
    previewCancellationConsequences,

    -- * Consequence executors
    applyImmediateConsequences,
    applyTerminalConsequences,

    -- * Charge computation (also used by soft-cancel + penalty previews)
    CancellationChargesOutcome (..),
    buildCancellationContext,
    customerCancellationChargesCalculation,
    getCancellationCharges,
    dropZeroCharge,

    -- * Location helpers
    driverDistanceToPickup,
    getDistanceToPickup,
  )
where

import qualified Data.HashMap.Strict as HM
import qualified Data.HashMap.Strict as HMS
import qualified Data.Map as M
import qualified Domain.Types.Booking as SRB
import qualified Domain.Types.BookingCancellationReason as SBCR
import qualified Domain.Types.CancellationConsequenceMatrix as DCCM
import qualified Domain.Types.CancellationReason as DTCR
import Domain.Types.DriverLocation
import qualified Domain.Types.Merchant as DMerc
import qualified Domain.Types.Ride as DRide
import qualified Domain.Types.TransporterConfig as DTC
import EulerHS.Prelude hiding (whenJust)
import Kernel.External.Maps
import Kernel.Prelude hiding (any, elem, map, mapM_, notElem)
import Kernel.Storage.Clickhouse.Config
import qualified Kernel.Storage.Clickhouse.Config as CH
import qualified Kernel.Storage.ClickhouseV2 as CHV2
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Streaming.Kafka.Producer.Types (HasKafkaProducer, KafkaProducerTools)
import Kernel.Types.Id
import Kernel.Types.Version (CloudType)
import Kernel.Utils.Common
import Lib.ConfigPilot.Interface.Types (getOneConfig)
import qualified Lib.DriverCoins.Coins as DC
import qualified Lib.DriverCoins.Types as DCT
import qualified Lib.DriverScore as DS
import qualified Lib.DriverScore.Types as DST
import qualified Lib.Finance.Core.Types as Finance
import Lib.Scheduler (SchedulerType)
import Lib.SessionizerMetrics.Types.Event
import qualified SharedLogic.Analytics as Analytics
import qualified SharedLogic.BehaviourManagement.CancellationRate as SCR
import qualified SharedLogic.BehaviourManagement.ConsequenceDispatcher as BehaviorDispatch
import SharedLogic.CallBAPInternal
import qualified SharedLogic.CallInternalMLPricing as ML
import qualified SharedLogic.CancellationConsequence as CancellationConsequence
import qualified SharedLogic.CancellationDues as SCD
import qualified SharedLogic.CancellationFault as CancellationFault
import qualified SharedLogic.CancellationSignals as CancellationSignals
import qualified SharedLogic.DriverCancellationPenalty as DCP
import qualified SharedLogic.DriverPool as DP
import qualified SharedLogic.External.LocationTrackingService.Flow as LF
import qualified SharedLogic.External.LocationTrackingService.Types as LT
import SharedLogic.GoogleTranslate (TranslateFlow)
import Storage.ConfigPilot.Config.TransporterConfig (TransporterConfigDimensions (..))
import qualified Storage.Queries.CancellationDuesDetails as QCDD
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.Ride as QRide
import qualified Storage.Queries.RiderDetails as QRiderDetails
import Tools.Error
import qualified Tools.Maps as Maps
import qualified Tools.Metrics as Metrics
import TransactionLogs.Types

-- | Everything a consequence executor may read, resolved exactly once per cancellation.
data CancellationDecision = CancellationDecision
  { signals :: CancellationSignals.CancellationSignals,
    faultVerdict :: Maybe CancellationFault.FaultVerdict,
    consequenceRow :: Maybe DCCM.CancellationConsequenceMatrix,
    cancelledBy :: DCT.CancellationType,
    reasonCode :: Maybe DTCR.CancellationReasonCode,
    disToPickup :: Maybe Meters
  }
  deriving (Generic)

-- | Call context shared by both executors; 'source' is the raw cancellation source
-- (ByUser / ByDriver / ByMerchant / ...) and gates the few source-specific legacy
-- behaviours (coin event only for user/driver cancels, DriverScore blocking only for
-- driver cancels, rider counters only for user cancels).
data ConsequenceCtx = ConsequenceCtx
  { merchant :: DMerc.Merchant,
    booking :: SRB.Booking,
    ride :: DRide.Ride,
    transporterConfig :: DTC.TransporterConfig,
    source :: SBCR.CancellationSource,
    decision :: CancellationDecision
  }
  deriving (Generic)

-- | Map the persisted cancellation source to the type the verdict rules / matrix / coin
-- engine consume. This is the ONLY place this mapping lives — flows must never hardcode
-- CancellationByDriver/CancellationByCustomer (ops/merchant cancels used to be
-- mislabelled as driver cancels that way).
cancellationSourceToType :: SBCR.CancellationSource -> DCT.CancellationType
cancellationSourceToType = \case
  SBCR.ByUser -> DCT.CancellationByCustomer
  SBCR.ByDriver -> DCT.CancellationByDriver
  SBCR.ByMerchant -> DCT.CancellationByMerchant
  SBCR.ByAllocator -> DCT.CancellationByAllocator
  SBCR.ByApplication -> DCT.CancellationByApplication
  SBCR.ByFleetOwner -> DCT.CancellationByFleetOwner

decideCancellationConsequences ::
  ( EsqDBFlow m r,
    CacheFlow m r,
    ClickhouseFlow m r
  ) =>
  SRB.Booking ->
  DRide.Ride ->
  DTC.TransporterConfig ->
  SBCR.CancellationSource ->
  Maybe DTCR.CancellationReasonCode ->
  Maybe Meters ->
  m CancellationDecision
decideCancellationConsequences booking ride transporterConfig source reasonCode disToPickup = do
  let cancelledBy = cancellationSourceToType source
  (signals, mbFaultVerdict) <- buildCancellationContext booking ride transporterConfig cancelledBy reasonCode disToPickup
  consequenceInput <- CancellationConsequence.buildConsequenceInputFromBooking booking mbFaultVerdict cancelledBy
  mbConsequenceRow <- CancellationConsequence.getOrResolveConsequence ride.id consequenceInput
  pure
    CancellationDecision
      { signals = signals,
        faultVerdict = mbFaultVerdict,
        consequenceRow = mbConsequenceRow,
        cancelledBy = cancelledBy,
        reasonCode = reasonCode,
        disToPickup = disToPickup
      }

-- | Consequences that apply to EVERY cancellation with an assigned ride, regardless of
-- whether the booking subsequently reallocates: a customer forced to cancel because the
-- driver never moved must still produce the driver-side consequences.
applyImmediateConsequences ::
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
    HasFlowEnv m r '["fabricGatewayBaseUrl" ::: BaseUrl],
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
  ConsequenceCtx ->
  Maybe Bool ->
  m ()
applyImmediateConsequences ctx doCancellationRateBasedBlocking = do
  -- consequences never block the cancel itself: any failure here is logged and dropped
  resultE <- withTryCatch "applyImmediateConsequences" $ do
    applyDriverRiderBlacklist
    applyDriverOverlayNotification
    applyDriverCoinEvent
    applyCancellationAnalytics
    driver <- QPerson.findById ctx.ride.driverId >>= fromMaybeM (PersonNotFound ctx.ride.driverId.getId)
    applyDriverMoneyConsequence driver
    applyDriverCancellationRateCount driver
  case resultE of
    Left err -> logError $ "applyImmediateConsequences failed for rideId " <> ctx.ride.id.getId <> ": " <> show err
    Right _ -> pure ()
  where
    row = ctx.decision.consequenceRow

    -- column: blacklistDriverForRiderSeconds — keep this driver out of this rider's pool
    -- (must run BEFORE any reallocation decision so the reallocated search excludes them)
    applyDriverRiderBlacklist =
      whenJust ((.blacklistDriverForRiderSeconds) =<< row) $ \blacklistTtl ->
        whenJust ctx.booking.riderId (DP.addDriverToRiderCancelledList blacklistTtl ctx.ride.driverId)

    -- column: driverNotificationKey — overlay to the driver explaining the consequence
    applyDriverOverlayNotification =
      whenJust ((.driverNotificationKey) =<< row) $ \overlayKey ->
        fork "cancellationConsequenceDriverNotify" $
          BehaviorDispatch.sendOverlayByKey (BehaviorDispatch.DispatchContext {merchantId = ctx.transporterConfig.merchantId, merchantOperatingCityId = ctx.booking.merchantOperatingCityId, counterConfig = Nothing, actionEvent = Nothing}) ctx.ride.driverId overlayKey

    -- column: driverDeduction (COIN variant, consumed inside the coin engine via the same
    -- cached matrix row) — fires for genuine customer/driver cancels only, never ops ones
    applyDriverCoinEvent =
      when (ctx.source == SBCR.ByUser || ctx.source == SBCR.ByDriver) $
        fork "cancellationConsequenceCoinEvent" $
          DC.driverCoinsEvent ctx.ride.driverId Nothing ctx.merchant.id ctx.booking.merchantOperatingCityId (DCT.Cancellation ctx.ride.createdAt ctx.booking.distanceToPickup ctx.decision.disToPickup ctx.decision.cancelledBy (fromMaybe (DTCR.CancellationReasonCode "OTHER") ctx.decision.reasonCode)) (Just ctx.ride.id.getId) ctx.ride.vehicleVariant (Just ctx.booking.vehicleServiceTier) (Just ctx.booking.configInExperimentVersions)

    -- source-based cancellation analytics + operator dashboard counters (moved from the
    -- retired RideCancel tag computation — the fault verdict is the judgment now)
    applyCancellationAnalytics =
      Analytics.updateCancellationAnalyticsAndDriverStats ctx.transporterConfig ctx.ride ctx.source

    -- column: driverDeduction (MoneyDeduction charges the driver via DriverFee/wallet;
    -- MoneyAddition credits them, wallet only — the adapter in CancellationConsequence
    -- turns the constructor into the signed amount DCP expects). Row-driven for every
    -- source, so customer-at-fault rows can compensate the driver with money.
    applyDriverMoneyConsequence driver =
      whenJust ((\r -> CancellationConsequence.driverMoneyDeduction r ctx.booking.estimatedFare) =<< row) $ \signedAmount ->
        fork "cancellationConsequenceDriverMoney" $ do
          let isWalletEnabled = fromMaybe False ctx.merchant.prepaidSubscriptionAndWalletEnabled || ctx.transporterConfig.driverWalletConfig.enableDriverWallet
          DCP.accumulateCancellationPenalty isWalletEnabled ctx.booking ctx.ride (Just signedAmount) ctx.transporterConfig driver

    -- column: countsTowardDriverCancellationRate — driver-initiated cancels go through the
    -- full DriverScore event (rate counter + repeat-offender blocking); customer-initiated
    -- cancels judged against the driver bump only the sliding-window counter.
    applyDriverCancellationRateCount driver = do
      let countsTowardRate = maybe False (.countsTowardDriverCancellationRate) row
      when (ctx.source == SBCR.ByDriver) $
        DS.driverScoreEventHandler ctx.ride.merchantOperatingCityId DST.OnDriverCancellation {countsTowardCancellationRate = countsTowardRate, merchantId = ctx.merchant.id, driver = driver, rideFare = Just ctx.booking.estimatedFare, currency = ctx.booking.currency, distanceUnit = ctx.booking.distanceUnit, doCancellationRateBasedBlocking = doCancellationRateBasedBlocking}
      when (ctx.source == SBCR.ByUser && countsTowardRate) $ do
        let windowSize = toInteger $ fromMaybe 7 ctx.transporterConfig.cancellationRateWindow
        void $ SCR.incrementCancelledCount ctx.ride.driverId windowSize

-- | Customer-side money consequences, applied on every cancelled ride — including when
-- the booking reallocates (the row decides whether the customer pays; on a reallocated
-- booking the charge lands as dues and is collected on the next ride). Owns: the charge
-- outcome (soft-cancel fee reuse for user cancels, else the matrix), the dues write
-- (SCD — the only dues writer), the ride charge total, the rider counters, and the
-- finance ledger entries (via the passed callback, so the ledger implementation stays
-- with the cancel internals without an import cycle).
--
-- Never throws: any failure is logged and reported as "no charge".
applyTerminalConsequences ::
  ( EncFlow m r,
    EsqDBReplicaFlow m r,
    CacheFlow m r,
    EsqDBFlow m r,
    HasKafkaProducer r,
    HasField "shortDurationRetryCfg" r RetryCfg,
    HasFlowEnv m r '["ltsCfg" ::: LT.LocationTrackingeServiceConfig],
    HasFlowEnv m r '["cloudType" ::: Maybe CloudType],
    HasField "serviceClickhouseCfg" r CH.ClickhouseCfg,
    HasField "serviceClickhouseEnv" r CH.ClickhouseEnv,
    CHV2.HasClickhouseEnv CHV2.APP_SERVICE_CLICKHOUSE m,
    ClickhouseFlow m r
  ) =>
  ConsequenceCtx ->
  -- | create finance ledger entries: base -> gst -> prepaid-balance debit -> m ()
  (HighPrecMoney -> HighPrecMoney -> Maybe HighPrecMoney -> m ()) ->
  m (Maybe CancellationChargesOutcome)
applyTerminalConsequences ctx createLedgerEntries = do
  let booking = ctx.booking
      ride = ctx.ride
      transporterConfig = ctx.transporterConfig
      decision = ctx.decision
  case booking.riderId of
    Nothing -> pure Nothing
    Just riderId -> do
      chargesE <- withTryCatch "applyTerminalConsequences" $ do
        riderDetails <- QRiderDetails.findById riderId >>= fromMaybeM (RiderDetailsNotFound riderId.getId)
        -- column: (implicit) rider lifetime cancel counter — customer-initiated cancels only
        when (ctx.source == SBCR.ByUser) $
          void $ QRiderDetails.updateCancelledRidesCount riderId.getId
        -- column: countsTowardCustomerCancellationStats
        when (maybe False (.countsTowardCustomerCancellationStats) decision.consequenceRow) $
          QRiderDetails.updateValidCancellationsCount riderId.getId
        -- columns: customerDeduction + customerCommissionAndTax + collectionMode
        mbExistingCdd <- QCDD.findByRideId ride.id
        mbOutcome <- case (ctx.source == SBCR.ByUser, ride.cancellationFeeIfCancelled) of
          (True, Just softCancelTotal) -> do
            mbFresh <- chargesOutcomeFromRow booking decision.consequenceRow
            pure $ case (mbFresh, decision.consequenceRow) of
              (Just fresh, Just row) ->
                let (base, tax) = CancellationConsequence.splitTaxInclusiveTotal row softCancelTotal
                    (_, commission) = CancellationConsequence.customerTaxAndCommission row base
                 in Just fresh {fee = Just base, tax = tax, commission = commission}
              _ ->
                Just
                  CancellationChargesOutcome
                    { fee = Just softCancelTotal,
                      tax = mbExistingCdd >>= (.cancellationFeeTax),
                      overdueFee = mbExistingCdd >>= (.overdueCancellationCharge),
                      overdueTax = mbExistingCdd >>= (.overdueCancellationTax),
                      commission = mbExistingCdd >>= (.cancellationCommission),
                      overdueCommission = mbExistingCdd >>= (.overdueCancellationCommission),
                      consequenceRowId = mbExistingCdd >>= (.cancellationConsequenceRowId),
                      collectionMode = mbExistingCdd >>= (.cancellationCollectionMode)
                    }
          _ ->
            if transporterConfig.canAddCancellationFee
              then do
                mbO <- customerCancellationChargesCalculation booking ride decision.cancelledBy decision.faultVerdict
                pure (dropZeroCharge <$> mbO)
              else pure Nothing
        whenJust mbOutcome $ \outcome ->
          whenJust outcome.fee $ \baseFee -> do
            let gst = fromMaybe 0 outcome.tax
                totalCharges = baseFee + gst
            logTagInfo ("bookingId-" <> booking.id.getId) ("cancellation charge applied: base=" <> show baseFee <> " tax=" <> show gst <> " row=" <> show outcome.consequenceRowId)
            -- a NEGATIVE total is a customer CREDIT (matrix addition): it only reduces
            -- outstanding dues inside SCD (clamped at zero) — no ride charge, no counters,
            -- no ledger entries
            when (totalCharges > 0) $
              QRide.updateCancellationChargesOnCancel (Just totalCharges) ride.id
            SCD.applyCancellationCharge
              SCD.ApplyCancellationChargeReq
                { ride = ride,
                  riderId = riderId,
                  currentDues = riderDetails.cancellationDues,
                  totalCharges = totalCharges,
                  currency = booking.currency,
                  cancellationFee = outcome.fee,
                  cancellationFeeTax = outcome.tax,
                  overdueCancellationCharge = outcome.overdueFee,
                  overdueCancellationTax = outcome.overdueTax,
                  cancellationCommission = outcome.commission,
                  overdueCancellationCommission = outcome.overdueCommission,
                  consequenceRowId = outcome.consequenceRowId,
                  collectionMode = outcome.collectionMode,
                  carryForwardEnabled = CancellationConsequence.shouldCarryForwardDues decision.consequenceRow
                }
            when (ctx.source == SBCR.ByUser && totalCharges > 0) $
              QRiderDetails.updateCancellationDueRidesCount riderId.getId
            let isWalletEnabled = fromMaybe False ctx.merchant.prepaidSubscriptionAndWalletEnabled || transporterConfig.driverWalletConfig.enableDriverWallet
            when (isWalletEnabled && totalCharges > 0) $
              createLedgerEntries baseFee gst ((\r -> CancellationConsequence.driverRideCreditDeduction r booking.estimatedFare) =<< decision.consequenceRow)
        pure mbOutcome
      case chargesE of
        Left err -> do
          logError $ "applyTerminalConsequences failed for rideId " <> ctx.ride.id.getId <> ": " <> show err
          pure Nothing
        Right res -> pure res

-- | Cancellation charge outcome for the customer side: the fee/tax pair, the reduced
--   overdue variants, and the platform's commission on each. Every field is optional —
--   a Nothing fee means no charge applies (downstream skips the dues/counter writes).
data CancellationChargesOutcome = CancellationChargesOutcome
  { fee :: Maybe HighPrecMoney,
    tax :: Maybe HighPrecMoney,
    overdueFee :: Maybe HighPrecMoney,
    overdueTax :: Maybe HighPrecMoney,
    commission :: Maybe HighPrecMoney,
    overdueCommission :: Maybe HighPrecMoney,
    -- id of the CancellationConsequenceMatrix row that produced this outcome (audit trail)
    consequenceRowId :: Maybe Text,
    -- show ConsequenceCollectionMode from the row; persisted on the dues row so the BAP
    -- handoff (immediate capture vs next-ride dues) can consume it later
    collectionMode :: Maybe Text
  }
  deriving (Generic, Show)

-- | Build the canonical cancellation signals and the (cached, once-per-ride) fault
-- verdict for a cancel flow. Callers fetch the driver's current distance to pickup
-- themselves so flows that already have it don't pay for a second LTS call.
buildCancellationContext ::
  ( EsqDBFlow m r,
    CacheFlow m r,
    ClickhouseFlow m r
  ) =>
  SRB.Booking ->
  DRide.Ride ->
  DTC.TransporterConfig ->
  DCT.CancellationType ->
  Maybe DTCR.CancellationReasonCode ->
  Maybe Meters ->
  m (CancellationSignals.CancellationSignals, Maybe CancellationFault.FaultVerdict)
buildCancellationContext booking ride transporterConfig cancelledBy reasonCode cancellationDisToPickup = do
  signals <- buildRideCancellationSignals booking ride transporterConfig cancellationDisToPickup
  mbFaultVerdict <-
    CancellationFault.getOrComputeFaultVerdict ride (Just booking.transactionId) transporterConfig.timeDiffFromUtc $
      CancellationFault.mkFaultVerdictData signals cancelledBy reasonCode
  pure (signals, mbFaultVerdict)

buildRideCancellationSignals ::
  ( EsqDBFlow m r,
    CacheFlow m r,
    ClickhouseFlow m r
  ) =>
  SRB.Booking ->
  DRide.Ride ->
  DTC.TransporterConfig ->
  Maybe Meters ->
  m CancellationSignals.CancellationSignals
buildRideCancellationSignals booking ride transporterConfig cancellationDisToPickup =
  CancellationSignals.buildCancellationSignals
    CancellationSignals.CancellationSignalsReq
      { ride = ride,
        quoteId = booking.quoteId,
        bookingCreatedAt = Just booking.createdAt,
        scheduledPickupTime = Just booking.startTime,
        fallbackDurationToPickup = booking.dqDurationToPickup,
        initialDisToPickup = booking.distanceToPickup,
        cancellationDisToPickup = cancellationDisToPickup,
        arrivedPickupThreshold = transporterConfig.arrivedPickupThreshold
      }

-- | Dry-run twin of 'decideCancellationConsequences' for previews (driver penalty check,
-- what-if screens): the SAME signals → fault verdict → matrix row pipeline, but WITHOUT
-- the per-ride Redis caches or the ride-row verdict persistence — the cancellation may
-- never happen, so nothing may be left behind.
previewCancellationConsequences ::
  ( EsqDBFlow m r,
    CacheFlow m r,
    ClickhouseFlow m r
  ) =>
  SRB.Booking ->
  DRide.Ride ->
  DTC.TransporterConfig ->
  SBCR.CancellationSource ->
  Maybe DTCR.CancellationReasonCode ->
  Maybe Meters ->
  m CancellationDecision
previewCancellationConsequences booking ride transporterConfig source reasonCode disToPickup = do
  let cancelledBy = cancellationSourceToType source
  signals <- buildRideCancellationSignals booking ride transporterConfig disToPickup
  mbFaultVerdict <-
    CancellationFault.computeFaultVerdictDryRun ride (Just booking.transactionId) transporterConfig.timeDiffFromUtc $
      CancellationFault.mkFaultVerdictData signals cancelledBy reasonCode
  consequenceInput <- CancellationConsequence.buildConsequenceInputFromBooking booking mbFaultVerdict cancelledBy
  mbConsequenceRow <- CancellationConsequence.resolveConsequence consequenceInput
  pure
    CancellationDecision
      { signals = signals,
        faultVerdict = mbFaultVerdict,
        consequenceRow = mbConsequenceRow,
        cancelledBy = cancelledBy,
        reasonCode = reasonCode,
        disToPickup = disToPickup
      }

-- | Real-cancel charge computation: resolves the matrix row through the per-ride cache
-- (so preview→cancel and charge→coin-fork all see the SAME row).
customerCancellationChargesCalculation ::
  ( EsqDBFlow m r,
    CacheFlow m r
  ) =>
  SRB.Booking ->
  DRide.Ride ->
  DCT.CancellationType ->
  Maybe CancellationFault.FaultVerdict ->
  m (Maybe CancellationChargesOutcome)
customerCancellationChargesCalculation booking ride cancellationType mbFaultVerdict = do
  transporterConfig <- getOneConfig (TransporterConfigDimensions {merchantOperatingCityId = booking.merchantOperatingCityId.getId}) Nothing >>= fromMaybeM (TransporterConfigNotFound booking.merchantOperatingCityId.getId)
  if transporterConfig.canAddCancellationFee
    then do
      -- The consequence matrix is AUTHORITATIVE (no JsonLogic fallback): the resolved row
      -- alone decides the charge; a miss means no charge (logged inside the resolver).
      -- Payment-method exemptions are Cash-dimension matrix rows now, not transporterConfig.
      consequenceInput <- CancellationConsequence.buildConsequenceInputFromBooking booking mbFaultVerdict cancellationType
      mbRow <- CancellationConsequence.getOrResolveConsequence ride.id consequenceInput
      chargesOutcomeFromRow booking mbRow
    else return Nothing

-- | Build the customer charge outcome from an (already resolved) matrix row — shared by
-- the real-cancel calculation and the dry-run soft-cancel preview.
chargesOutcomeFromRow ::
  (MonadFlow m) =>
  SRB.Booking ->
  Maybe DCCM.CancellationConsequenceMatrix ->
  m (Maybe CancellationChargesOutcome)
chargesOutcomeFromRow booking = \case
  Nothing -> pure Nothing
  Just row -> do
    let breakup = CancellationConsequence.computeCustomerCharge row booking.estimatedFare
    logTagInfo ("bookingId-" <> getId booking.id) ("consequence matrix row " <> row.id.getId <> ": fee=" <> show breakup.fee <> " tax=" <> show breakup.tax <> " commission=" <> show breakup.commission <> " overdue=" <> show breakup.overdueFee)
    pure $
      Just
        CancellationChargesOutcome
          { fee = breakup.fee,
            tax = breakup.tax,
            overdueFee = breakup.overdueFee,
            overdueTax = Nothing,
            commission = breakup.commission,
            overdueCommission = Nothing,
            consequenceRowId = Just row.id.getId,
            collectionMode = show <$> row.collectionMode
          }

-- | A computed-but-zero total charge means "nothing to collect": drop the fee (and with it
-- the commission) so downstream skips the dues/counter writes, but keep the overdue fields.
dropZeroCharge :: CancellationChargesOutcome -> CancellationChargesOutcome
dropZeroCharge o = case o.fee of
  Just charges
    | charges + fromMaybe 0 o.tax == 0 ->
      CancellationChargesOutcome {fee = Nothing, tax = Nothing, overdueFee = o.overdueFee, overdueTax = o.overdueTax, commission = Nothing, overdueCommission = o.overdueCommission, consequenceRowId = o.consequenceRowId, collectionMode = o.collectionMode}
  _ -> o

-- | Compute (without applying) the customer cancellation charge — used by the
-- soft-cancel preview (API.Beckn.Cancel). Fully DRY-RUN: no Redis caches, no ride-row
-- verdict persistence, no cached matrix row. A preview must never freeze a verdict for a
-- cancellation that may not happen — riders opening the cancel screen used to persist a
-- customer-attributed verdict (typically early_customer_cancel) that a later REAL cancel,
-- even a driver one, then reused from the cache.
getCancellationCharges ::
  ( EsqDBFlow m r,
    CacheFlow m r,
    EncFlow m r,
    HasKafkaProducer r,
    EsqDBReplicaFlow m r,
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
  SBCR.CancellationSource ->
  Maybe DTCR.CancellationReasonCode ->
  m (Maybe CancellationChargesOutcome)
getCancellationCharges booking ride source reasonCode = do
  transporterConfig <- getOneConfig (TransporterConfigDimensions {merchantOperatingCityId = booking.merchantOperatingCityId.getId}) Nothing >>= fromMaybeM (TransporterConfigNotFound booking.merchantOperatingCityId.getId)
  case booking.riderId of
    Nothing -> return Nothing
    Just _rid ->
      if transporterConfig.canAddCancellationFee
        then do
          (cancellationDisToPickup, _mbLocation) <- getDistanceToPickup booking (Just ride)
          decision <- previewCancellationConsequences booking ride transporterConfig source reasonCode cancellationDisToPickup
          mbOutcome <- chargesOutcomeFromRow booking decision.consequenceRow
          return (dropZeroCharge <$> mbOutcome)
        else return Nothing

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
    EsqDBReplicaFlow m r
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
