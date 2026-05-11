{-
  AggregatedCommission scheduler.

  Per-merchant job that rolls per-ride Commission invoices into one
  AggregatedCommission invoice per fleet owner per calendar period (DAILY /
  WEEKLY / MONTHLY).

  Key design points:
    * Calendar-aligned periods (computed in merchant timezone, persisted as UTC).
    * jobData carries the frequency in effect when scheduled — protects the
      in-flight period from mid-cycle config changes; current config drives the
      NEXT schedule.
    * lastAggThrough is derived from finance_invoice (max periodEnd of prior
      AggregatedCommission rows (fleetowner wise) including Voided markers) — no extra state.
    * Per-merchant job, inner fleet-owner/driver loop. Discovery uses recent
      Commission ∪ AggregatedCommission rows so marker-only fleets stay covered.
    * Empty period → 0-amount marker invoice (status=Voided) so lastAggThrough
      keeps advancing; markers are hidden from list/PDF APIs by default.
-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module SharedLogic.Allocator.Jobs.AggregatedCommissionInvoiceCreation.AggregatedCommissionInvoiceCreation
  ( runAggregatedCommissionInvoiceCreationJob,
    -- Re-exported for the bootstrap dashboard endpoint so it computes the
    -- first period boundaries with the same calendar logic as the scheduler.
    endOfNextPeriodAfter,
    startOfPeriodContaining,
    schedulerSafetyOffsetSeconds,
  )
where

import Data.IORef (newIORef, readIORef, writeIORef)
import Data.List (nub)
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
import Kernel.Types.Error
import Kernel.Types.Id (Id (..))
import Kernel.Utils.Common
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
import qualified Storage.Queries.FleetOwnerInformation as QFOI
import qualified Storage.Queries.Person as QPerson

-- | Bound on how far back to look when discovering active recipients. Wide
-- enough that a recipient idle for ~3 months is still picked up, narrow
-- enough that long-dead recipients fall off naturally and stop generating
-- markers.
discoveryLookbackDays :: Integer
discoveryLookbackDays = 90

-- | Buffer added to the calendar boundary before firing — gives in-flight
-- ride-end commission inserts time to settle so they aren't missed by an
-- aggregation that fires exactly at midnight.
schedulerSafetyOffsetSeconds :: NominalDiffTime
schedulerSafetyOffsetSeconds = 300 -- 5 minutes

-- | Used when invoiceConfig.commissionAggregationFrequency is unset.
defaultFrequency :: DTC.CommissionAggregationFrequency
defaultFrequency = DTC.MONTHLY

runAggregatedCommissionInvoiceCreationJob ::
  ( BeamFlow m r,
    CacheFlow m r,
    EsqDBFlow m r,
    MonadFlow m,
    MonadIO m,
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
runAggregatedCommissionInvoiceCreationJob Job {id, jobInfo} = withLogTag ("JobId-" <> id.getId) $ do
  let jobData = jobInfo.jobData
      mId = jobData.merchantId
      mocId = jobData.merchantOperatingCityId
      embeddedFreq = jobData.frequency
      scheduledStart = jobData.periodStart
      scheduledEnd = jobData.periodEnd
      lockKey =
        "AggCommJob:" <> mocId.getId <> ":" <> show scheduledEnd

  resultRef <- liftIO $ newIORef Complete
  mbResult <- Hedis.whenWithLockRedisAndReturnValue lockKey 1800 $ do
    now <- getCurrentTime
    merchant <- CQM.findById mId >>= fromMaybeM (MerchantNotFound mId.getId)
    transporterConfig <-
      SCTC.findByMerchantOpCityId mocId Nothing
        >>= fromMaybeM (TransporterConfigNotFound mocId.getId)

    let mbInvoiceConfig = transporterConfig.invoiceConfig
        enabled = fromMaybe False (mbInvoiceConfig >>= (.commissionAggregationEnabled))

    if not enabled
      then do
        logInfo $ "Commission aggregation disabled for merchantOpCity " <> mocId.getId <> "; skipping run and not rescheduling"
        liftIO $ writeIORef resultRef Complete
      else do
        let newFreq = fromMaybe defaultFrequency (mbInvoiceConfig >>= (.commissionAggregationFrequency))
            tzOffset = transporterConfig.timeDiffFromUtc
        mocCity <- CQMOC.findById mocId >>= fromMaybeM (MerchantOperatingCityNotFound mocId.getId)
        let merchantAddress = Just (show mocCity.city <> ", " <> show mocCity.state <> ", " <> show mocCity.country)
            currency = mocCity.currency

        recipients <- discoverRecipients mocId now
        logInfo $
          "AggregatedCommission run for merchantOpCity " <> mocId.getId <> " (embedded=" <> show embeddedFreq <> ", current=" <> show newFreq <> "): "
            <> show (length recipients)
            <> " recipients discovered"

        forM_ recipients $ \recipient ->
          processRecipient
            merchant
            merchantAddress
            currency
            mocId
            scheduledStart
            scheduledEnd
            embeddedFreq
            newFreq
            tzOffset
            recipient
            now

        scheduleNextRun mId mocId newFreq tzOffset now
        liftIO $ writeIORef resultRef Complete

  case mbResult of
    Left () -> pure Complete -- another worker holds the lock; treat as done
    Right () -> liftIO $ readIORef resultRef

-- | Union of recipients with recent Commission OR AggregatedCommission rows.
-- Both DRIVER and FLEET_OWNER supported. The (id, type) pair drives entity
-- lookup downstream (Person vs FleetOwnerInformation). Marker-only recipients
-- included so their cursors keep advancing.
discoverRecipients ::
  (BeamFlow m r, MonadFlow m) =>
  Id DMOC.MerchantOperatingCity ->
  UTCTime ->
  m [(Text, BeckInvoice.IssuedToType)]
discoverRecipients mocId now = do
  let lookbackStart = addUTCTime (negate (fromIntegral discoveryLookbackDays * 86400)) now
  commissions <-
    QFinanceInvoiceExtra.findCommissionInvoicesInRange
      mocId.getId
      lookbackStart
      now
      Nothing
  aggInvoices <-
    QFinanceInvoiceExtra.findAggregatedCommissionInvoicesInRange
      mocId.getId
      lookbackStart
      now
      Nothing
  let pairsCommissions = map (\inv -> (inv.issuedToId, inv.issuedToType)) commissions
      pairsAgg = map (\inv -> (inv.issuedToId, inv.issuedToType)) aggInvoices
  pure $ nub (pairsCommissions <> pairsAgg)

-- | Supplier-side fields resolved per recipient, mirroring the recipient-type
-- branch in 'SharedLogic.Finance.Wallet.buildFinanceCtx'. For FLEET_OWNER,
-- pulled from FleetOwnerInformation. For DRIVER, only the display name is
-- populated (drivers don't carry GST/VAT/address as suppliers in this codebase
-- — same as the per-ride Commission flow).
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
  m (Maybe RecipientInfo)
resolveRecipientInfo BeckInvoice.FLEET_OWNER recipientId = do
  mbFleetInfo <- QFOI.findByPrimaryKey (Id recipientId)
  pure $
    mbFleetInfo <&> \f ->
      RecipientInfo
        { riName = f.fleetName,
          riAddress = f.stripeAddress <&> Wallet.formatStripeAddress,
          riGstin = f.gstNumberDec,
          riVatNumber = f.vatNumber,
          riPanNumberDec = f.panNumberDec
        }
resolveRecipientInfo BeckInvoice.DRIVER recipientId = do
  mbPerson <- QPerson.findById (Id recipientId)
  pure $
    mbPerson <&> \p ->
      RecipientInfo
        { riName = Just (p.firstName <> maybe "" (" " <>) p.lastName),
          riAddress = Nothing,
          riGstin = Nothing,
          riVatNumber = Nothing,
          riPanNumberDec = Nothing
        }
resolveRecipientInfo _ _ = pure Nothing -- RIDER / CUSTOMER never receive Commission invoices

processRecipient ::
  ( BeamFlow m r,
    CacheFlow m r,
    EsqDBFlow m r,
    MonadFlow m
  ) =>
  DM.Merchant ->
  Maybe Text -> -- merchant address (issuedByAddress)
  Currency ->
  Id DMOC.MerchantOperatingCity ->
  UTCTime -> -- jobData.periodStart
  UTCTime -> -- jobData.periodEnd
  DTC.CommissionAggregationFrequency -> -- embedded freq
  DTC.CommissionAggregationFrequency -> -- new freq (current config)
  Seconds -> -- tz offset
  (Text, BeckInvoice.IssuedToType) -> -- (recipientId, recipientType)
  UTCTime -> -- now
  m ()
processRecipient merchant merchantAddress currency mocId scheduledStart scheduledEnd embeddedFreq newFreq tzOffset (recipientId, recipientType) now = do
  mbInfo <- resolveRecipientInfo recipientType recipientId
  case mbInfo of
    Nothing ->
      logError $
        "AggregatedCommission: recipient lookup failed, skipping ("
          <> show recipientType
          <> ", "
          <> recipientId
          <> ")"
    Just info -> do
      lastAggThrough <- QFinanceInvoiceExtra.findLatestAggregatedCommissionPeriodEnd mocId.getId recipientId
      let cursor0 = case lastAggThrough of
            Nothing -> scheduledStart
            Just t -> addUTCTime 1 t
      -- Phase 1: cover the scheduled window using EMBEDDED freq, splitting any
      -- gap into one invoice per embedded-freq period.
      cursor1 <-
        if cursor0 <= scheduledEnd
          then do
            let phase1Periods = enumeratePeriods cursor0 scheduledEnd embeddedFreq tzOffset
            forM_ phase1Periods $ \(pStart, pEnd) ->
              emitInvoice merchant merchantAddress currency mocId info recipientId recipientType pStart pEnd
            pure $ case lastNonEmpty phase1Periods of
              Just (_, pEnd) -> addUTCTime 1 pEnd
              Nothing -> cursor0
          else pure cursor0 -- idempotency: scheduled period already covered
          -- Phase 2: backfill any newer COMPLETED periods using NEW freq
      let phase2Periods = enumerateCompletedPeriods cursor1 now newFreq tzOffset
      forM_ phase2Periods $ \(pStart, pEnd) ->
        emitInvoice merchant merchantAddress currency mocId info recipientId recipientType pStart pEnd

-- | Emit either a real AggregatedCommission invoice (when commissions exist)
-- or a 0-amount Voided marker (when the period is empty for this recipient) —
-- the marker keeps lastAggThrough advancing. issuedToType is preserved from
-- the recipient (DRIVER or FLEET_OWNER) so downstream PDF / list APIs render
-- the right party.
emitInvoice ::
  ( BeamFlow m r,
    CacheFlow m r,
    EsqDBFlow m r,
    MonadFlow m
  ) =>
  DM.Merchant ->
  Maybe Text ->
  Currency ->
  Id DMOC.MerchantOperatingCity ->
  RecipientInfo ->
  Text -> -- recipientId (issuedToId)
  BeckInvoice.IssuedToType -> -- recipientType (issuedToType)
  UTCTime -> -- pStart
  UTCTime -> -- pEnd
  m ()
emitInvoice merchant merchantAddress currency mocId info recipientId recipientType pStart pEnd = do
  commissions <-
    QFinanceInvoiceExtra.findCommissionInvoicesInRange
      mocId.getId
      pStart
      pEnd
      (Just recipientId)
  let isMarker = null commissions
      isVat = isJust info.riVatNumber || isJust merchant.vatNumber
      lineItems = if isMarker then [] else map mkLineItem commissions
      input =
        InvoiceI.InvoiceInput
          { invoiceType = BeckInvoice.AggregatedCommission,
            paymentOrderId = Nothing,
            issuedToType = recipientType,
            issuedToId = recipientId,
            issuedToName = info.riName,
            issuedToAddress = info.riAddress,
            issuedByType = "MERCHANT",
            issuedById = merchant.id.getId,
            issuedByName = Just merchant.name,
            issuedByAddress = merchantAddress,
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
            lineItems = lineItems,
            gstBreakdown = Nothing, -- per-line VAT already booked on underlying Commission rows; nothing new to record
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
    Left err ->
      logError $
        "AggregatedCommission: failed to create invoice for "
          <> show recipientType
          <> " "
          <> recipientId
          <> " period "
          <> show pStart
          <> "->"
          <> show pEnd
          <> ": "
          <> show err
    Right inv -> do
      when isMarker $
        InvoiceSvc.updateInvoiceStatus inv.id FInvoice.Voided
      logInfo $
        "AggregatedCommission: "
          <> (if isMarker then "marker " else "")
          <> inv.invoiceNumber
          <> " for "
          <> show recipientType
          <> " "
          <> recipientId
          <> " ["
          <> show pStart
          <> ", "
          <> show pEnd
          <> "]"

-- | One line item per underlying per-ride Commission invoice. Description
-- includes the ride/booking reference where available so the rolled-up
-- consolidated PDF preserves per-ride traceability. We deliberately leave
-- 'descriptionType' as Nothing — otherwise the renderer would replace our
-- per-ride description with a single repeated localized "Platform Commission"
-- string for every row, losing the ride distinction the consolidated PDF
-- needs.
mkLineItem :: FInvoice.Invoice -> InvoiceI.InvoiceLineItem
mkLineItem inv =
  InvoiceI.InvoiceLineItem
    { description = "Platform commission - " <> fromMaybe inv.invoiceNumber inv.referenceId,
      descriptionType = Nothing,
      quantity = 1,
      unitPrice = inv.totalAmount,
      lineTotal = inv.totalAmount,
      isExternalCharge = False,
      groupId = Just "g-commission",
      itemType = Just InvoiceI.Fare
    }

-- | Schedule the next merchant-level run at the next calendar period boundary
-- under CURRENT freq. Embeds CURRENT freq in the next jobData so it acts as
-- the embedded freq when that run fires.
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
  DTC.CommissionAggregationFrequency ->
  Seconds ->
  UTCTime ->
  m ()
scheduleNextRun mId mocId newFreq tzOffset now = do
  let nextEnd = endOfNextPeriodAfter now newFreq tzOffset
      nextStart = startOfPeriodContaining nextEnd newFreq tzOffset
      fireAt = addUTCTime schedulerSafetyOffsetSeconds nextEnd
      scheduleAfter = diffUTCTime fireAt now
      jobData =
        AggregatedCommissionInvoiceCreationJobData
          { merchantId = mId,
            merchantOperatingCityId = mocId,
            periodStart = nextStart,
            periodEnd = nextEnd,
            frequency = newFreq
          }
  when (scheduleAfter > 0) $ do
    JC.createJobIn @_ @'AggregatedCommissionInvoiceCreation (Just mId) (Just mocId) scheduleAfter jobData
    logInfo $ "Scheduled next AggregatedCommission run: fireAt=" <> show fireAt <> " period=[" <> show nextStart <> ", " <> show nextEnd <> "] freq=" <> show newFreq

-- | Enumerate all calendar-aligned periods of @freq@ that lie in @[cursor, cap]@.
-- Final period is capped to @cap@ (so a partial period bridging into a mode
-- switch lands as one invoice rather than overshooting the scheduled window).
enumeratePeriods ::
  UTCTime -> -- inclusive start (cursor)
  UTCTime -> -- inclusive cap
  DTC.CommissionAggregationFrequency ->
  Seconds ->
  [(UTCTime, UTCTime)]
enumeratePeriods cursor cap freq tzOffset
  | cursor > cap = []
  | otherwise =
    let pEnd = endOfNextPeriodAfter (addUTCTime (-1) cursor) freq tzOffset
        cappedEnd = min pEnd cap
     in (cursor, cappedEnd) : enumeratePeriods (addUTCTime 1 cappedEnd) cap freq tzOffset

-- | Enumerate calendar-aligned periods of @freq@ starting at @cursor@ that have
-- already COMPLETED by @now@. Stops at the first period whose end is in the future.
enumerateCompletedPeriods ::
  UTCTime ->
  UTCTime -> -- now
  DTC.CommissionAggregationFrequency ->
  Seconds ->
  [(UTCTime, UTCTime)]
enumerateCompletedPeriods cursor now freq tzOffset =
  let pEnd = endOfNextPeriodAfter (addUTCTime (-1) cursor) freq tzOffset
   in if pEnd > now
        then []
        else (cursor, pEnd) : enumerateCompletedPeriods (addUTCTime 1 pEnd) now freq tzOffset

-- | Last element of a list, or Nothing for empty. Pure pattern-match version
-- avoids any Prelude/Kernel.Prelude qualifier games.
lastNonEmpty :: [a] -> Maybe a
lastNonEmpty [] = Nothing
lastNonEmpty [x] = Just x
lastNonEmpty (_ : xs) = lastNonEmpty xs

-- | End-of-period semantics: the smallest "period end" timestamp that is
-- STRICTLY GREATER than @t@. All math is done in merchant-local time then
-- converted back to UTC, so DAILY/WEEKLY/MONTHLY boundaries align to local
-- midnights / Sundays / month-ends regardless of UTC offset.
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

-- | Start of the calendar period that contains @t@.
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

-- | Move a period-end day forward by exactly one period.
nextPeriodDay :: Day -> DTC.CommissionAggregationFrequency -> Day
nextPeriodDay d = \case
  DTC.DAILY -> addDays 1 d
  DTC.WEEKLY -> addDays 7 d
  DTC.MONTHLY ->
    let (yr, mo, _) = toGregorian d
        (nextYr, nextMo) = if mo == 12 then (yr + 1, 1) else (yr, mo + 1)
     in fromGregorian nextYr nextMo (gregorianMonthLength nextYr nextMo)
