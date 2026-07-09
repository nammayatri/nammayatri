{-
  AggregatedCommission scheduler. Per-(recipient, period) job chain emitting one
  consolidated invoice per calendar period (DAILY/WEEKLY/MONTHLY). Bootstrap
  happens at fleet/driver onboarding.

  TZ caveat: period bounds use static 'transporterConfig.timeDiffFromUtc' (not
  DST-aware, codebase-wide pattern). For DST zones (e.g. Helsinki +2/+3), admin
  must flip the offset on the last Sun of March (+2 -> +3) and last Sun of
  October (+3 -> +2); otherwise bounds drift 1h for half the year.
-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module SharedLogic.Allocator.Jobs.AggregatedCommissionInvoiceCreation.AggregatedCommissionInvoiceCreation
  ( runAggregatedCommissionInvoiceCreationJob,
    bootstrapAggregatedCommissionChain,
  )
where

import qualified Data.Aeson as A
import qualified Data.Map.Strict as M
import Data.Time.Calendar (Day, addDays, fromGregorian, gregorianMonthLength, toGregorian)
import Data.Time.Calendar.WeekDate (toWeekDate)
import Data.Time.Clock (UTCTime (..), secondsToDiffTime)
import qualified "beckn-spec" Domain.Types.Invoice as BeckInvoice
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.TransporterConfig as DTC
import Kernel.Beam.Lib.UtilsTH (HasSchemaName)
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Id (Id (..))
import Kernel.Utils.Common
import Lib.ConfigPilot.Interface.Types (getOneConfig)
import qualified Lib.Finance.Core.Types as Finance
import qualified Lib.Finance.Domain.Types.Invoice as FInvoice
import qualified Lib.Finance.Invoice.Interface as InvoiceI
import qualified Lib.Finance.Invoice.Service as InvoiceSvc
import Lib.Finance.Storage.Beam.BeamFlow (BeamFlow)
import qualified Lib.Finance.Storage.Queries.InvoiceExtra as QFinanceInvoiceExtra
import Lib.Scheduler
import Lib.Scheduler.JobStorageType.DB.Table (SchedulerJobT)
import qualified Lib.Scheduler.JobStorageType.SchedulerType as JC
import SharedLogic.Allocator (AggregatedCommissionInvoiceCreationJobData (..), AllocatorJobType (..))
import qualified SharedLogic.Finance.Wallet as Wallet
import Storage.Beam.SchedulerJob ()
import qualified Storage.Cac.TransporterConfig as SCTC
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import Storage.ConfigPilot.Config.TransporterConfig (TransporterConfigDimensions (..))
import qualified Storage.Queries.FleetOwnerInformation as QFOI
import qualified Storage.Queries.Person as QPerson
import Tools.Error

-- | Buffer past the calendar boundary so in-flight ride-end commission inserts settle.
schedulerSafetyOffsetSeconds :: NominalDiffTime
schedulerSafetyOffsetSeconds = 300

defaultFrequency :: DTC.CommissionAggregationFrequency
defaultFrequency = DTC.MONTHLY

-- | Default batch size for paginated Commission fetches. Override via
-- 'invoiceConfig.commissionAggregationBatchSize'.
defaultCommissionAggregationBatchSize :: Int
defaultCommissionAggregationBatchSize = 500

runAggregatedCommissionInvoiceCreationJob ::
  ( BeamFlow m r,
    CacheFlow m r,
    EsqDBFlow m r,
    Finance.HasActorInfo m r,
    HasShortDurationRetryCfg r c,
    HasField "maxShards" r Int,
    HasField "schedulerSetName" r Text,
    HasField "schedulerType" r SchedulerType,
    HasField "jobInfoMap" r (M.Map Text Bool),
    HasField "blackListedJobs" r [Text],
    JobCreatorEnv r,
    HasSchemaName SchedulerJobT
  ) =>
  Job 'AggregatedCommissionInvoiceCreation ->
  m ExecutionResult
runAggregatedCommissionInvoiceCreationJob Job {id, jobInfo} = withLogTag ("JobId-" <> id.getId) do
  let jobData = jobInfo.jobData
      mId = jobData.merchantId
      mocId = jobData.merchantOperatingCityId
      issuedToId = jobData.issuedToId
      issuedToType = jobData.issuedToType
      periodStart = jobData.periodStart
      periodEnd = jobData.periodEnd
      lockKey = "AggCommJob:" <> mocId.getId <> ":" <> issuedToId <> ":" <> show periodEnd
      recipient = show issuedToType <> " " <> issuedToId

  -- Lock per (mocId, issuedToId, periodEnd) — defends against duplicate-chain firings
  -- that the Allocator's per-row lock can't catch.
  result <-
    Hedis.whenWithLockRedisAndReturnValue lockKey 1800 $ do
      transporterConfig <-
        getOneConfig (TransporterConfigDimensions {merchantOperatingCityId = mocId.getId}) (Just (SCTC.findByMerchantOpCityId mocId Nothing))
          >>= fromMaybeM (TransporterConfigNotFound mocId.getId)
      let mbInvoiceConfig = transporterConfig.invoiceConfig
          enabled = fromMaybe False (mbInvoiceConfig >>= (.commissionAggregationEnabled))

      if not enabled
        then pure $ Terminate $ "AggCom: commissionAggregationEnabled=false for mocId " <> mocId.getId <> " for " <> recipient <> "; chain ends"
        else do
          lastPeriodEnd <- QFinanceInvoiceExtra.findLatestAggregatedCommissionPeriodEnd mocId.getId issuedToId
          case lastPeriodEnd of
            Just lastEnd
              | periodEnd <= lastEnd ->
                pure $ Terminate $ "AggCom: jobData.periodEnd " <> show periodEnd <> " <= lastAggThrough " <> show lastEnd <> " for " <> recipient <> "; period already covered"
            Just lastEnd
              | periodStart <= lastEnd ->
                pure $ Terminate $ "AggCom: jobData.periodStart " <> show periodStart <> " <= lastAggThrough " <> show lastEnd <> " for " <> recipient <> "; periodStart overlaps already-covered range"
            _ -> do
              tryEmitInvoice mId mocId issuedToId issuedToType periodStart periodEnd mbInvoiceConfig
              scheduleNextRun mId mocId issuedToId issuedToType periodEnd transporterConfig
              pure Complete

  case result of
    Right r -> pure r
    Left _ -> pure Complete -- another firing holds the lock and is processing this period; nothing to do

tryEmitInvoice ::
  ( BeamFlow m r,
    CacheFlow m r,
    EsqDBFlow m r,
    Finance.HasActorInfo m r
  ) =>
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  Text ->
  BeckInvoice.IssuedToType ->
  UTCTime ->
  UTCTime ->
  Maybe DTC.InvoiceConfig ->
  m ()
tryEmitInvoice mId mocId issuedToId issuedToType periodStart periodEnd mbInvoiceConfig = do
  rinfo <- resolveRecipientInfo issuedToType issuedToId
  merchant <- CQM.findById mId >>= fromMaybeM (MerchantNotFound mId.getId)
  mocCity <- CQMOC.findById mocId >>= fromMaybeM (MerchantOperatingCityNotFound mocId.getId)
  -- Seller name/address are config-only — no merchant fallback. If unset, the
  -- PDF seller block just shows live-fetched business ID + VAT.
  let sellerName = mbInvoiceConfig >>= (.invoiceSellerName)
      sellerAddress = mbInvoiceConfig >>= (.invoiceSellerAddress)
  emitInvoice merchant sellerName sellerAddress mocCity.currency mocId rinfo issuedToId issuedToType periodStart periodEnd mbInvoiceConfig

-- | Supplier-side fields per recipient. FLEET_OWNER pulls from FleetOwnerInformation;
-- DRIVER only carries the display name (no address/tax IDs at supplier level).
data RecipientInfo = RecipientInfo
  { riName :: Maybe Text,
    riAddress :: Maybe Text,
    riGstin :: Maybe Text,
    riVatNumber :: Maybe Text,
    riPanNumberDec :: Maybe Text
  }

resolveRecipientInfo ::
  (CacheFlow m r, EsqDBFlow m r, MonadFlow m) =>
  BeckInvoice.IssuedToType ->
  Text ->
  m RecipientInfo
resolveRecipientInfo BeckInvoice.FLEET_OWNER recipientId = do
  f <- QFOI.findByPrimaryKey (Id recipientId) >>= fromMaybeM (FleetOwnerNotFound recipientId)
  pure
    RecipientInfo
      { riName = f.fleetName,
        riAddress = f.stripeAddress <&> Wallet.formatStripeAddress,
        riGstin = f.gstNumberDec,
        riVatNumber = f.vatNumber,
        riPanNumberDec = f.panNumberDec
      }
resolveRecipientInfo BeckInvoice.DRIVER recipientId = do
  p <- QPerson.findById (Id recipientId) >>= fromMaybeM (PersonNotFound recipientId)
  pure
    RecipientInfo
      { riName = Just (p.firstName <> maybe "" (" " <>) p.lastName),
        riAddress = Nothing,
        riGstin = Nothing,
        riVatNumber = Nothing,
        riPanNumberDec = Nothing
      }
resolveRecipientInfo other _ =
  -- Bootstrap only enqueues DRIVER/FLEET_OWNER chains; reaching RIDER/CUSTOMER is an invariant violation.
  throwError $ InternalError $ "AggCom: unexpected issuedToType " <> show other

-- | Build + persist an AggregatedCommission row when the period has any underlying
-- per-ride Commissions. Empty period → silent no-op (no marker invoices).
-- Commissions streamed in batches (config or default) to bound per-query DB load.
emitInvoice ::
  ( BeamFlow m r,
    CacheFlow m r,
    EsqDBFlow m r,
    Finance.HasActorInfo m r
  ) =>
  DM.Merchant ->
  Maybe Text -> -- sellerName
  Maybe Text -> -- sellerAddress
  Currency ->
  Id DMOC.MerchantOperatingCity ->
  RecipientInfo ->
  Text -> -- recipientId
  BeckInvoice.IssuedToType ->
  UTCTime ->
  UTCTime ->
  Maybe DTC.InvoiceConfig ->
  m ()
emitInvoice merchant sellerName sellerAddress currency mocId info recipientId recipientType pStart pEnd mbInvoiceConfig = do
  let batchSize = fromMaybe defaultCommissionAggregationBatchSize (mbInvoiceConfig >>= (.commissionAggregationBatchSize))
  commissions <- fetchAllCommissionsInRange mocId.getId pStart pEnd (Just recipientId) batchSize
  if null commissions
    then logInfo $ "AggCom: empty period for " <> show recipientType <> " " <> recipientId <> " [" <> show pStart <> ", " <> show pEnd <> "]; skipping emit"
    else do
      let isVat = isJust info.riVatNumber || isJust merchant.vatNumber
          input =
            InvoiceI.InvoiceInput
              { invoiceType = BeckInvoice.AggregatedCommission,
                entityReferenceId = Nothing,
                referenceInvoiceNumber = Nothing,
                issuedToType = recipientType,
                issuedToId = recipientId,
                issuedToName = info.riName,
                issuedToAddress = info.riAddress,
                issuedByType = "MERCHANT",
                issuedById = merchant.id.getId,
                issuedByName = sellerName,
                issuedByAddress = sellerAddress,
                supplierName = info.riName,
                supplierAddress = info.riAddress,
                supplierGSTIN = info.riGstin,
                supplierTaxNo = info.riVatNumber,
                supplierId = Just recipientId,
                merchantGstin = merchant.gstin,
                referenceId = Nothing,
                gstinOfParty = info.riGstin,
                panOfParty = info.riPanNumberDec,
                panType = Nothing,
                counterpartyId = recipientId,
                tdsRateReason = Nothing,
                tanOfDeductee = Nothing,
                lineItems = concatMap flattenInvoice commissions,
                gstBreakdown = Nothing, -- per-line VAT already booked on underlying Commission rows
                currency = currency,
                dueAt = Nothing,
                periodStart = Just pStart,
                periodEnd = Just pEnd,
                merchantId = merchant.id.getId,
                merchantOperatingCityId = mocId.getId,
                merchantShortId = merchant.shortId.getShortId,
                isVat = isVat,
                issuedToTaxNo = info.riVatNumber,
                issuedByTaxNo = merchant.vatNumber,
                paymentMode = Nothing
              }
      result <- InvoiceSvc.createInvoice input []
      case result of
        Left err -> throwError $ InternalError $ "AggCom: createInvoice failed for " <> show recipientType <> " " <> recipientId <> " [" <> show pStart <> ", " <> show pEnd <> "]: " <> show err
        Right inv -> logInfo $ "AggCom: " <> inv.invoiceNumber <> " for " <> show recipientType <> " " <> recipientId <> " [" <> show pStart <> ", " <> show pEnd <> "]"

-- | Re-emit every line item of an underlying Commission invoice as-is — the agg_comm JL sums
-- per descriptionType tag, and a summary-line-per-invoice would collapse base+VAT and
-- commission-vs-cancellation rows. Unparseable line_items ⇒ one untyped line with the invoice total.
flattenInvoice :: FInvoice.Invoice -> [InvoiceI.InvoiceLineItem]
flattenInvoice inv =
  case (A.fromJSON inv.lineItems :: A.Result [InvoiceI.InvoiceLineItem]) of
    A.Success items | not (null items) -> map withRefSuffix items
    _ ->
      [ InvoiceI.InvoiceLineItem
          { description = "Platform commission" <> refSuffix,
            descriptionType = Nothing,
            quantity = 1,
            unitPrice = inv.totalAmount,
            lineTotal = inv.totalAmount,
            isExternalCharge = False,
            groupId = Just "g-commission",
            itemType = Just InvoiceI.Fare
          }
      ]
  where
    refSuffix = " - " <> fromMaybe inv.invoiceNumber inv.referenceId
    withRefSuffix li =
      InvoiceI.InvoiceLineItem
        { description = li.description <> refSuffix,
          descriptionType = li.descriptionType,
          quantity = li.quantity,
          unitPrice = li.unitPrice,
          lineTotal = li.lineTotal,
          isExternalCharge = li.isExternalCharge,
          groupId = li.groupId,
          itemType = li.itemType
        }

-- | Page through Commission rows for [from, to] in chunks of @batchSize@.
-- Terminates on empty page (codebase null-termination convention).
fetchAllCommissionsInRange ::
  (BeamFlow m r) =>
  Text -> -- mocId
  UTCTime -> -- from
  UTCTime -> -- to
  Maybe Text -> -- recipientId
  Int -> -- batchSize
  m [FInvoice.Invoice]
fetchAllCommissionsInRange mocId from to mbRecipientId batchSize = go 0 []
  where
    go offset acc = do
      batch <- QFinanceInvoiceExtra.findCommissionInvoicesInRange mocId from to mbRecipientId (Just batchSize) (Just offset)
      if null batch
        then pure acc
        else go (offset + batchSize) (acc <> batch)

-- | Queue the next-period job under the CURRENT config frequency (mode-switches
-- take effect here). Past-due 'scheduleAfter' clamped to 0 so the Allocator
-- picks it up on its next poll for cascade catch-up.
scheduleNextRun ::
  ( BeamFlow m r,
    CacheFlow m r,
    EsqDBFlow m r,
    JobCreatorEnv r,
    HasSchemaName SchedulerJobT,
    HasField "schedulerType" r SchedulerType,
    MonadFlow m
  ) =>
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  Text ->
  BeckInvoice.IssuedToType ->
  UTCTime -> -- previous job's periodEnd
  DTC.TransporterConfig ->
  m ()
scheduleNextRun mId mocId issuedToId issuedToType prevPeriodEnd transporterConfig = do
  let mbInvoiceConfig = transporterConfig.invoiceConfig
      currentFreq = fromMaybe defaultFrequency (mbInvoiceConfig >>= (.commissionAggregationFrequency))
      tzOffset = transporterConfig.timeDiffFromUtc
      nextStart = addUTCTime 1 prevPeriodEnd
      nextEnd = endOfNextPeriodAfter (addUTCTime (-1) nextStart) currentFreq tzOffset
      fireAt = addUTCTime schedulerSafetyOffsetSeconds nextEnd
  enqueueJob mId mocId issuedToId issuedToType nextStart nextEnd fireAt currentFreq "scheduled next"

-- | Bootstrap the chain at fleet/driver onboarding (also reachable via the admin
-- '/scheduler/trigger' endpoint). Anchors the first job to the calendar period
-- containing 'now'. No bootstrap-time idempotency check — runtime checks in
-- 'runAggregatedCommissionInvoiceCreationJob' handle accidental duplicate chains.
bootstrapAggregatedCommissionChain ::
  ( BeamFlow m r,
    CacheFlow m r,
    EsqDBFlow m r,
    MonadFlow m,
    JobCreatorEnv r,
    HasSchemaName SchedulerJobT,
    HasField "schedulerType" r SchedulerType
  ) =>
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  Text ->
  BeckInvoice.IssuedToType ->
  m ()
bootstrapAggregatedCommissionChain mId mocId issuedToId issuedToType = do
  transporterConfig <-
    getOneConfig (TransporterConfigDimensions {merchantOperatingCityId = mocId.getId}) (Just (SCTC.findByMerchantOpCityId mocId Nothing))
      >>= fromMaybeM (TransporterConfigNotFound mocId.getId)
  let mbInvoiceConfig = transporterConfig.invoiceConfig
      enabled = fromMaybe False (mbInvoiceConfig >>= (.commissionAggregationEnabled))
  if not enabled
    then logInfo $ "AggCom disabled for mocId " <> mocId.getId <> "; bootstrap skipped for " <> show issuedToType <> " " <> issuedToId
    else do
      let freq = fromMaybe defaultFrequency (mbInvoiceConfig >>= (.commissionAggregationFrequency))
          tzOffset = transporterConfig.timeDiffFromUtc
      now <- getCurrentTime
      let periodEnd = endOfNextPeriodAfter now freq tzOffset
          periodStart = startOfPeriodContaining periodEnd freq tzOffset
          fireAt = addUTCTime schedulerSafetyOffsetSeconds periodEnd
      enqueueJob mId mocId issuedToId issuedToType periodStart periodEnd fireAt freq "bootstrapped"

enqueueJob ::
  ( BeamFlow m r,
    CacheFlow m r,
    EsqDBFlow m r,
    JobCreatorEnv r,
    HasSchemaName SchedulerJobT,
    HasField "schedulerType" r SchedulerType,
    MonadFlow m
  ) =>
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  Text ->
  BeckInvoice.IssuedToType ->
  UTCTime ->
  UTCTime ->
  UTCTime ->
  DTC.CommissionAggregationFrequency ->
  Text -> -- log verb (e.g. "scheduled next" / "bootstrapped")
  m ()
enqueueJob mId mocId issuedToId issuedToType periodStart periodEnd fireAt freq verb = do
  now <- getCurrentTime
  let scheduleAfter = max 0 (diffUTCTime fireAt now)
      jobData =
        AggregatedCommissionInvoiceCreationJobData
          { merchantId = mId,
            merchantOperatingCityId = mocId,
            issuedToId = issuedToId,
            issuedToType = issuedToType,
            periodStart = periodStart,
            periodEnd = periodEnd
          }
  JC.createJobIn @_ @'AggregatedCommissionInvoiceCreation (Just mId) (Just mocId) scheduleAfter jobData
  logInfo $ "AggCom " <> verb <> " for " <> show issuedToType <> " " <> issuedToId <> ": fireAt=" <> show fireAt <> " period=[" <> show periodStart <> ", " <> show periodEnd <> "] freq=" <> show freq

-- | Smallest period-end timestamp strictly greater than @t@. Computed in merchant-local
-- time then converted back to UTC so DAILY/WEEKLY/MONTHLY align to local midnights / Sundays / month-ends.
endOfNextPeriodAfter ::
  UTCTime ->
  DTC.CommissionAggregationFrequency ->
  Seconds ->
  UTCTime
endOfNextPeriodAfter tUtc freq tzOffset =
  let tzDiff = secondsToNominalDiffTime tzOffset
      tLocal = addUTCTime tzDiff tUtc
      localDay = utctDay tLocal
      eod = secondsToDiffTime 86399 -- 23:59:59
      candidateDay = case freq of
        DTC.DAILY -> localDay
        DTC.WEEKLY ->
          let (_, _, dow) = toWeekDate localDay -- ISO: Mon=1..Sun=7
              daysToSunday = toInteger (7 - dow)
           in addDays daysToSunday localDay
        DTC.MONTHLY ->
          let (yr, mo, _) = toGregorian localDay
              eomDay = gregorianMonthLength yr mo
           in fromGregorian yr mo eomDay
      candidateLocalUtc = UTCTime candidateDay eod
      finalDay =
        if candidateLocalUtc > tLocal
          then candidateDay
          else nextPeriodDay candidateDay freq
      finalLocalUtc = UTCTime finalDay eod
   in addUTCTime (negate tzDiff) finalLocalUtc

-- | Start of the calendar period containing @t@.
startOfPeriodContaining ::
  UTCTime ->
  DTC.CommissionAggregationFrequency ->
  Seconds ->
  UTCTime
startOfPeriodContaining tUtc freq tzOffset =
  let tzDiff = secondsToNominalDiffTime tzOffset
      tLocal = addUTCTime tzDiff tUtc
      localDay = utctDay tLocal
      startDay = case freq of
        DTC.DAILY -> localDay
        DTC.WEEKLY ->
          let (_, _, dow) = toWeekDate localDay
           in addDays (negate (toInteger (dow - 1))) localDay
        DTC.MONTHLY ->
          let (yr, mo, _) = toGregorian localDay
           in fromGregorian yr mo 1
      startLocalUtc = UTCTime startDay 0
   in addUTCTime (negate tzDiff) startLocalUtc

nextPeriodDay :: Day -> DTC.CommissionAggregationFrequency -> Day
nextPeriodDay d = \case
  DTC.DAILY -> addDays 1 d
  DTC.WEEKLY -> addDays 7 d
  DTC.MONTHLY ->
    let (yr, mo, _) = toGregorian d
        (nextYr, nextMo) = if mo == 12 then (yr + 1, 1) else (yr, mo + 1)
     in fromGregorian nextYr nextMo (gregorianMonthLength nextYr nextMo)
