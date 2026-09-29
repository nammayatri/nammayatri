{-# OPTIONS_GHC -Wno-orphans #-}

-- NOTE (reviewer, remove before merge): Main's file, extended:
--   1. refundRegistrationAmount refuses a city that pays through a bulk partner (HDFC CBX); Juspay/Stripe as on main.
--   2. The scheduled-config upsert validates the merged config, creates / moves / replaces the queued sweep job, and
--      clears the config cache.
--   3. New read-only VIEW / VIEW_DIFF for that config (viewScheduledPayoutConfigs, diffScheduledPayoutConfig).
--   The sweep config and sweep job are the same for every payout partner, so (2) applies to Juspay/Stripe cities too, on
--   purpose. Two known Low side effects are explained at the code below.
module Domain.Action.Dashboard.PayoutRequest
  ( deleteVpa,
    updateVpa,
    refundRegistrationAmount,
    upsertScheduledPayoutConfig,
    UpdateScheduledPayoutConfigReq (..),
    viewScheduledPayoutConfigs,
    diffScheduledPayoutConfig,
    ConfigFieldDiff (..),
    ConfigScheduleEffect (..),
  )
where

import qualified Data.Text as T
import qualified Domain.Types.DriverInformation as DI
import qualified Domain.Types.Extra.MerchantServiceConfig as DEMSC
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Person as DP
import qualified Domain.Types.ScheduledPayoutConfig as DSPC
import qualified Domain.Types.VehicleCategory as DV
import qualified Environment
import EulerHS.Prelude hiding (id, readMaybe, sum)
import qualified Kernel.External.Payment.Interface as Payment
import qualified Kernel.External.Payment.Interface.Types as KT
import qualified Kernel.External.Payout.Interface as IPayout
import Kernel.Prelude
import Kernel.Types.APISuccess (APISuccess (Success))
import qualified Kernel.Types.Beckn.Context
import Kernel.Types.Error
import qualified Kernel.Types.Id as Id
import Kernel.Utils.Common
import Lib.ConfigPilot.Interface.Getter (invalidateConfigInMem)
import Lib.ConfigPilot.Interface.Types (ConfigDimensions (getConfig), getOneConfig)
import Lib.Finance.Storage.Beam.BeamFlow (BeamFlow)
import Lib.Payment.API.Payout (VerifyVpaFlow (..))
import qualified Lib.Payment.API.Payout.Types as PayoutTypes
import qualified Lib.Payment.Domain.Action as Payout
import qualified Lib.Payment.Domain.Types.Common as DPayment
import qualified Lib.Payment.Domain.Types.PayoutBatch as DPayoutBatch
import qualified Lib.Payment.Payout.Registration as Registration
import qualified Lib.Yudhishthira.Types.ConfigPilot as CP
import SharedLogic.Allocator.Jobs.Payout.ScheduledBatchPayout (computeNextRunTime)
import SharedLogic.Payout.RetimeScheduledBatchPayout (RetimeDecision (..), previewResume, previewRetime, resumeSweepJob, retimeQueuedPayoutJob, scheduleChanged)
import Storage.Beam.Payment ()
import qualified Storage.CachedQueries.Merchant as QM
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import Storage.ConfigPilot.Config.PayoutConfig (PayoutConfigDimensions (..))
import Storage.ConfigPilot.Config.ScheduledPayoutConfig (ScheduledPayoutConfigDimensions (..))
import qualified Storage.Queries.DriverInformation as QDI
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.ScheduledPayoutConfig as QSPC
import qualified Tools.Payment as TPayment
import qualified Tools.Payout as TP

deleteVpa :: PayoutTypes.DeleteVpaReq -> Environment.Flow APISuccess
deleteVpa req = do
  let driverIds = (Id.Id <$> req.personIds) :: [Id.Id DP.Person]
  void $ QDI.updatePayoutVpaAndStatusByDriverIds Nothing Nothing driverIds
  pure Success

updateVpa :: PayoutTypes.UpdateVpaReq -> Environment.Flow APISuccess
updateVpa req = do
  let driverId = (Id.Id req.personId) :: Id.Id DP.Person
  person <- QPerson.findById driverId >>= fromMaybeM (PersonNotFound driverId.getId)
  let resolvedStatus = fromMaybe (defaultStatus req.verify) (parseVpaStatus req.vpaStatus)
  QDI.updatePayoutVpaAndStatus (Just req.vpa) (Just resolvedStatus) person.id
  pure Success
  where
    defaultStatus shouldVerify =
      if shouldVerify then DI.VERIFIED_BY_USER else DI.MANUALLY_ADDED

    parseVpaStatus = \case
      Nothing -> Nothing
      Just statusText -> readMaybe (T.unpack statusText)

instance VerifyVpaFlow Environment.Flow where
  verifyVpaForUpdate = verifyVpaForUpdateImpl

verifyVpaForUpdateImpl :: PayoutTypes.UpdateVpaReq -> Environment.Flow ()
verifyVpaForUpdateImpl req = do
  let driverId = (Id.Id req.personId) :: Id.Id DP.Person
  person <- QPerson.findById driverId >>= fromMaybeM (PersonNotFound driverId.getId)
  paymentServiceName <- TPayment.decidePaymentService (DEMSC.PaymentService Payment.Juspay) person.clientSdkVersion person.merchantOperatingCityId
  let verifyVPAReq =
        KT.VerifyVPAReq
          { orderId = Nothing,
            customerId = Just person.id.getId,
            vpa = req.vpa
          }
      verifyVpaCall = TPayment.verifyVpa person.merchantId person.merchantOperatingCityId paymentServiceName (Just person.id.getId)
  resp <- withTryCatch "verifyVPAService:updateVpa" $ Payout.verifyVPAService verifyVPAReq verifyVpaCall
  case resp of
    Left e -> throwError $ InvalidRequest $ "VPA Verification Failed: " <> show e
    Right response ->
      unless (response.status == "VALID") $
        throwError $ InvalidRequest $ "Invalid VPA Updation: " <> show response

refundRegistrationAmount :: (BeamFlow Environment.Flow Environment.AppEnv) => Id.ShortId DM.Merchant -> Kernel.Types.Beckn.Context.City -> PayoutTypes.RefundRegAmountReq -> Environment.Flow APISuccess
refundRegistrationAmount merchantShortId opCity req = do
  let driverId = (Id.Id req.personId) :: Id.Id DP.Person
  driverInfo <- QDI.findById driverId >>= fromMaybeM (PersonNotFound driverId.getId)
  driver <- QPerson.findById driverId >>= fromMaybeM (PersonNotFound driverId.getId)

  merchant <- QM.findByShortId merchantShortId >>= fromMaybeM (MerchantDoesNotExist merchantShortId.getShortId)
  merchantOpCity <- CQMOC.findByMerchantIdAndCity merchant.id opCity >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchant-Id-" <> merchant.id.getId <> "-city-" <> show opCity)

  (payoutServiceFlow, payoutServiceName, mbPersonBankAccount) <- TP.getCreatePayoutServiceFlow TP.MerchantServiceUsageConfigOption DEMSC.PayoutService driver.clientSdkVersion merchantOpCity.id driverId
  -- NOTE (reviewer, remove before merge): Bulk-only refusal. A bulk payout partner (HDFC CBX) pays only inside a batch,
  --   and this refund has no batch, so it would write an order that is never sent. Juspay and Stripe are unchanged.
  when (payoutServiceFlow == IPayout.BulkFlow) $
    throwError $ InvalidRequest "Registration refund is not supported in a city that pays through a bulk payout partner"

  -- Eligibility check: VPA must be captured via webhook, refund not already done
  let payoutVpaValid = case payoutServiceFlow of
        IPayout.JuspayFlow -> driverInfo.payoutVpaStatus == Just DI.VIA_WEBHOOK && isJust driverInfo.payoutVpa
        IPayout.StripeFlow -> True
        -- NOTE (reviewer, remove before merge): Case arm for the new BulkFlow constructor, never reached (refused above).
        IPayout.BulkFlow -> False
  unless (payoutVpaValid && isNothing driverInfo.payoutRegAmountRefunded) $
    throwError $ InvalidRequest $ "Driver not eligible for refund | driver id: " <> show driverId

  -- Get the registration PaymentOrder ID
  registrationOrderId <- case driverInfo.payoutRegistrationOrderId of
    Just oid -> pure $ Id.Id oid
    Nothing -> throwError $ InvalidRequest $ "No registration order found for driver: " <> show driverId

  -- Get payout call
  let vehicleCategory = DV.AUTO_CATEGORY
  payoutConfig <- getOneConfig (PayoutConfigDimensions {merchantOperatingCityId = merchantOpCity.id.getId, vehicleCategory = Just vehicleCategory, isPayoutEnabled = Nothing}) Nothing >>= fromMaybeM (PayoutConfigNotFound (show vehicleCategory) merchantOpCity.id.getId)
  let createPayoutOrderCall = TP.createPayoutOrder payoutServiceName merchantOpCity.id driverId mbPersonBankAccount

  -- Delegate to lib — it looks up PaymentOrder for amount + VPA, checks idempotency
  logDebug $ "Refunding registration for driverId: " <> driverId.getId <> " | orderId: " <> registrationOrderId.getId
  mbResult <- Registration.refundRegistrationAmount registrationOrderId createPayoutOrderCall payoutConfig.remark payoutConfig.orderType (show merchantOpCity.city) payoutServiceFlow

  case mbResult of
    Just _ -> do
      -- Mark refund on DriverInformation (domain concern)
      QDI.updatePayoutRegAmountRefunded (Just Registration.registrationAmount) driverId
    Nothing ->
      logDebug $ "Registration refund already exists for driver: " <> driverId.getId

  pure Success

--------------------------------------------------------------------------------
-- Scheduled Payout Config Admin API
--------------------------------------------------------------------------------

-- NOTE (reviewer, remove before merge): New optional fields: intervalHours / intervalDays (for the new HOURLY /
--   EVERY_N_DAYS frequencies), rescheduleBufferMinutes / bufferCheckEnabled (move the queued job on a timing edit),
--   itemsPerBatchLimit / defaultPayoutRail (read only by bulk payouts), and bulkStatusCheckIntervalMinutes /
--   bulkStatusCheckBatchLimit (read only by the city's bulk status-check job). An absent field keeps the stored value
--   (update) or stays empty (create), so a Juspay/Stripe caller that does not send them sees main's behaviour.
data UpdateScheduledPayoutConfigReq = UpdateScheduledPayoutConfigReq
  { payoutCategory :: DPayment.EntityName,
    isEnabled :: Maybe Bool,
    frequency :: Maybe DSPC.ScheduledPayoutFrequency,
    dayOfWeek :: Maybe Int,
    dayOfMonth :: Maybe Int,
    timeOfDay :: Maybe Text,
    batchSize :: Maybe Int,
    minimumPayoutAmount :: Maybe HighPrecMoney,
    maxRetriesPerDriver :: Maybe Int,
    vehicleCategory :: Maybe DV.VehicleCategory,
    remark :: Maybe Text,
    orderType :: Maybe Text,
    timeDiffFromUtc :: Maybe Seconds,
    intervalHours :: Maybe Int,
    intervalDays :: Maybe Int,
    rescheduleBufferMinutes :: Maybe Int,
    bufferCheckEnabled :: Maybe Bool,
    itemsPerBatchLimit :: Maybe Int,
    defaultPayoutRail :: Maybe DPayoutBatch.PayoutBatchRail,
    bulkStatusCheckIntervalMinutes :: Maybe Int,
    bulkStatusCheckBatchLimit :: Maybe Int
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

-- NOTE (reviewer, remove before merge): Shared with Juspay/Stripe. With bufferCheckEnabled unset (every existing row) or
--   false, a timing edit applies after the queued run, as on main. Only bufferCheckEnabled = true moves the queued job.

-- | Whether committing @updated@ over @existing@ moves the already-queued job. The single rule for
--   both the commit and the VIEW_DIFF preview, so the preview cannot promise a move the commit skips.
--   A resume is handled by 'resumeSweepJob' instead, which moves or creates the job.
willRetimeOnCommit :: DSPC.ScheduledPayoutConfig -> DSPC.ScheduledPayoutConfig -> Bool
willRetimeOnCommit existing updated =
  updated.bufferCheckEnabled == Just True && updated.isEnabled && existing.isEnabled && scheduleChanged existing updated

-- NOTE (reviewer, remove before merge): Shared with Juspay/Stripe, changed on purpose. Main merges the request onto the
--   stored row and saves: no validation, no sweep-job handling, no cache clear.
--   Update: merge (same as main) -> validate -> on a resume (disabled -> enabled) resumeSweepJob moves or creates the
--   sweep job -> on a timing edit with bufferCheckEnabled, move the queued job -> save -> clear the config cache.
--   Create: validate -> if enabled, resumeSweepJob -> save -> clear the cache. Job work runs before the save, so an error
--   saves nothing. The upsert owns the sweep job: ops must not also queue it with merchant/scheduler/trigger, or a
--   second job is queued. Validation runs on the merged row, so a stored row that fails it (e.g. timeOfDay "2:00")
--   cannot be edited, not even disabled, unless the same call fixes that field.
upsertScheduledPayoutConfig ::
  Id.ShortId DM.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  UpdateScheduledPayoutConfigReq ->
  Environment.Flow APISuccess
upsertScheduledPayoutConfig merchantShortId opCity req = do
  merchant <- QM.findByShortId merchantShortId >>= fromMaybeM (MerchantDoesNotExist merchantShortId.getShortId)
  merchantOpCity <- CQMOC.findByMerchantIdAndCity merchant.id opCity >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchant-Id-" <> merchant.id.getId <> "-city-" <> show opCity)
  mbExisting <- getOneConfig (ScheduledPayoutConfigDimensions {merchantOperatingCityId = merchantOpCity.id.getId, isEnabled = Nothing, payoutCategory = Just req.payoutCategory}) Nothing
  case mbExisting of
    Just existing -> do
      now <- getCurrentTime
      let newIsEnabled = fromMaybe existing.isEnabled req.isEnabled
          isResuming = newIsEnabled && not existing.isEnabled
          updated = applyConfigUpdate req now existing
      validateScheduledPayoutConfig updated
      -- Move the queued job first. If that fails, the error goes back to the caller and nothing is
      -- saved, so the config and the job never disagree.
      when isResuming $
        resumeScheduledBatchPayoutJob updated
      -- Move the already-queued job to the new schedule (only when bufferCheckEnabled; otherwise the
      -- edit applies after the queued run).
      when (willRetimeOnCommit existing updated) $
        retimeQueuedPayoutJob existing updated
      QSPC.updateByPrimaryKey updated
      clearScheduledPayoutConfigCache
      logInfo $ "Updated ScheduledPayoutConfig for " <> show req.payoutCategory <> " in city " <> merchantOpCity.id.getId
    Nothing -> do
      now <- getCurrentTime
      let newConfig =
            DSPC.ScheduledPayoutConfig
              { DSPC.merchantId = merchant.id,
                DSPC.merchantOperatingCityId = merchantOpCity.id,
                DSPC.payoutCategory = req.payoutCategory,
                DSPC.isEnabled = fromMaybe False req.isEnabled,
                DSPC.frequency = fromMaybe DSPC.DAILY req.frequency,
                DSPC.dayOfWeek = req.dayOfWeek,
                DSPC.dayOfMonth = req.dayOfMonth,
                DSPC.timeOfDay = fromMaybe "02:00" req.timeOfDay,
                DSPC.batchSize = fromMaybe 50 req.batchSize,
                DSPC.minimumPayoutAmount = fromMaybe 10.0 req.minimumPayoutAmount,
                DSPC.maxRetriesPerDriver = fromMaybe 3 req.maxRetriesPerDriver,
                DSPC.vehicleCategory = req.vehicleCategory,
                DSPC.remark = req.remark,
                DSPC.orderType = fromMaybe "FULFILL_ONLY" req.orderType,
                DSPC.timeDiffFromUtc = fromMaybe 19800 req.timeDiffFromUtc,
                DSPC.itemsPerBatchLimit = req.itemsPerBatchLimit,
                DSPC.defaultPayoutRail = req.defaultPayoutRail,
                DSPC.intervalHours = req.intervalHours,
                DSPC.intervalDays = req.intervalDays,
                DSPC.rescheduleBufferMinutes = req.rescheduleBufferMinutes,
                DSPC.bufferCheckEnabled = req.bufferCheckEnabled,
                DSPC.bulkStatusCheckIntervalMinutes = req.bulkStatusCheckIntervalMinutes,
                DSPC.bulkStatusCheckBatchLimit = req.bulkStatusCheckBatchLimit,
                DSPC.createdAt = now,
                DSPC.updatedAt = now
              }
      -- NOTE (reviewer, remove before merge): Known Low issue. QSPC.create writes the row to Redis (KV) first; Postgres gets it
      --   after the drainer runs. A second save inside that lag reads "no config" (config-pilot reads Postgres), takes this
      --   create path again, and the drainer drops that insert (ON CONFLICT DO NOTHING): the save is lost. Main loses such a
      --   save for about 2 h after a create (it never clears the cache); here only within the drain lag, but a lost enabled
      --   save also creates or moves the sweep job. When that job runs it reads the stored config again (not the lost
      --   values), and the next save (update path) moves the job and makes Redis and Postgres agree.
      validateScheduledPayoutConfig newConfig
      -- The job first, so an error saves nothing.
      when newConfig.isEnabled $
        resumeScheduledBatchPayoutJob newConfig
      QSPC.create newConfig
      clearScheduledPayoutConfigCache
      logInfo $ "Created ScheduledPayoutConfig for " <> show req.payoutCategory <> " in city " <> merchantOpCity.id.getId
  pure Success

-- NOTE (reviewer, remove before merge): Main does this merge inline in the upsert, with the same rules for the existing
--   fields (no Juspay/Stripe change). A function here so VIEW_DIFF merges exactly like the save.

-- | The one place a dashboard request is merged onto a stored config. Shared by the commit above and
--   by the VIEW_DIFF preview below: a preview that could merge differently from the upsert would be
--   worse than no preview.
--
--   Present fields win, absent fields keep what is stored ('<|>' for the optional ones, 'fromMaybe'
--   for the mandatory).
applyConfigUpdate :: UpdateScheduledPayoutConfigReq -> UTCTime -> DSPC.ScheduledPayoutConfig -> DSPC.ScheduledPayoutConfig
applyConfigUpdate req now existing =
  existing
    { DSPC.isEnabled = newIsEnabled,
      DSPC.frequency = fromMaybe existing.frequency req.frequency,
      DSPC.dayOfWeek = req.dayOfWeek <|> existing.dayOfWeek,
      DSPC.dayOfMonth = req.dayOfMonth <|> existing.dayOfMonth,
      DSPC.timeOfDay = fromMaybe existing.timeOfDay req.timeOfDay,
      DSPC.batchSize = fromMaybe existing.batchSize req.batchSize,
      DSPC.minimumPayoutAmount = fromMaybe existing.minimumPayoutAmount req.minimumPayoutAmount,
      DSPC.maxRetriesPerDriver = fromMaybe existing.maxRetriesPerDriver req.maxRetriesPerDriver,
      DSPC.vehicleCategory = req.vehicleCategory <|> existing.vehicleCategory,
      DSPC.remark = req.remark <|> existing.remark,
      DSPC.orderType = fromMaybe existing.orderType req.orderType,
      DSPC.timeDiffFromUtc = fromMaybe existing.timeDiffFromUtc req.timeDiffFromUtc,
      DSPC.intervalHours = req.intervalHours <|> existing.intervalHours,
      DSPC.intervalDays = req.intervalDays <|> existing.intervalDays,
      DSPC.rescheduleBufferMinutes = req.rescheduleBufferMinutes <|> existing.rescheduleBufferMinutes,
      DSPC.bufferCheckEnabled = req.bufferCheckEnabled <|> existing.bufferCheckEnabled,
      DSPC.itemsPerBatchLimit = req.itemsPerBatchLimit <|> existing.itemsPerBatchLimit,
      DSPC.defaultPayoutRail = req.defaultPayoutRail <|> existing.defaultPayoutRail,
      DSPC.bulkStatusCheckIntervalMinutes = req.bulkStatusCheckIntervalMinutes <|> existing.bulkStatusCheckIntervalMinutes,
      DSPC.bulkStatusCheckBatchLimit = req.bulkStatusCheckBatchLimit <|> existing.bulkStatusCheckBatchLimit,
      DSPC.updatedAt = now
    }
  where
    newIsEnabled = fromMaybe existing.isEnabled req.isEnabled

--------------------------------------------------------------------------------
-- Reading a config, and previewing an edit to it
--------------------------------------------------------------------------------

-- | One field an edit would change, rendered for display. Text on both sides because the fields have
--   a dozen different types and the caller only shows them.
data ConfigFieldDiff = ConfigFieldDiff
  { field :: Text,
    from :: Maybe Text,
    to :: Maybe Text
  }

-- | What an edit would do to the sweep's timing -- the part of a diff an operator actually acts on.
data ConfigScheduleEffect = ConfigScheduleEffect
  { currentNextRunAt :: UTCTime,
    proposedNextRunAt :: UTCTime,
    decision :: RetimeDecision,
    bufferMinutes :: Maybe Int
  }

-- NOTE (reviewer, remove before merge): New read-only API (GET payout/payout/scheduledPayoutConfig, command VIEW or
--   VIEW_DIFF); nothing is written. Main has no such route; Juspay/Stripe cities can use it like any other.
--   Known Low issue: a VIEW made while a just-created row is still only in Redis caches "no config" in config-pilot for
--   up to 2 h, or until the next save clears it. With payoutCategory set, the upsert and the sweep read that same cache
--   entry. Main uses the same caching pattern.

-- | The stored config(s) for a city, optionally narrowed to one payout category.
--
--   Read through config-pilot, the way 'upsertScheduledPayoutConfig' reads them and the way the sweep
--   itself does -- not through the raw query -- so the screen cannot show a config the job would not
--   use.
viewScheduledPayoutConfigs ::
  Id.ShortId DM.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Maybe DPayment.EntityName ->
  Environment.Flow [DSPC.ScheduledPayoutConfig]
viewScheduledPayoutConfigs merchantShortId opCity mbPayoutCategory = do
  merchantOpCityId <- resolveOpCityId merchantShortId opCity
  getConfig (ScheduledPayoutConfigDimensions {merchantOperatingCityId = merchantOpCityId, isEnabled = Nothing, payoutCategory = mbPayoutCategory}) Nothing

-- | What committing this request would change, without changing anything: the merged config, the
--   fields that differ, and the effect on the queued job.
--
--   Every part of the answer comes from the function the commit path uses -- 'applyConfigUpdate' for
--   the merge, 'computeNextRunTime' for the timings, 'previewRetime' for the queued job -- so the
--   preview cannot disagree with the commit.
diffScheduledPayoutConfig ::
  Id.ShortId DM.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  UpdateScheduledPayoutConfigReq ->
  Environment.Flow (DSPC.ScheduledPayoutConfig, [ConfigFieldDiff], ConfigScheduleEffect)
diffScheduledPayoutConfig merchantShortId opCity req = do
  merchantOpCityId <- resolveOpCityId merchantShortId opCity
  existing <-
    getOneConfig (ScheduledPayoutConfigDimensions {merchantOperatingCityId = merchantOpCityId, isEnabled = Nothing, payoutCategory = Just req.payoutCategory}) Nothing
      >>= fromMaybeM (InvalidRequest $ "No scheduled payout config for " <> show req.payoutCategory <> " in this city yet; there is nothing to diff against")
  now <- getCurrentTime
  let proposed = applyConfigUpdate req now existing
  -- Validated here too: a preview that hides the error the commit would raise is a trap.
  validateScheduledPayoutConfig proposed
  scheduledFromCurrent <- computeNextRunTime existing
  scheduledFromProposed <- computeNextRunTime proposed
  -- The queued job is looked up whether or not the commit would move it: it is what actually
  -- fires next, so it answers both "when does it run now" and, unless the commit moves it, "when
  -- does it run after the edit".
  lookedUp <- previewRetime existing proposed
  -- A resume (turning the schedule back on) moves or creates the job through 'resumeSweepJob'.
  let isResuming = proposed.isEnabled && not existing.isEnabled
  resumeDecision <- if isResuming then Just <$> previewResume proposed else pure Nothing
  let mbQueuedAt = case lookedUp of
        NoQueuedJob -> Nothing
        LeaveInsideBuffer queuedAt -> Just queuedAt
        MoveJobTo queuedAt _ -> Just queuedAt
        NotMoved mbAt -> mbAt
      decision = case resumeDecision of
        Just resumeEffect -> resumeEffect
        Nothing -> if willRetimeOnCommit existing proposed then lookedUp else NotMoved mbQueuedAt
      currentNextRunAt = fromMaybe scheduledFromCurrent mbQueuedAt
      -- A job that stays queued fires at its own time and only then schedules from the new config;
      -- the new config's own next time is the answer only when the commit moves or creates the job.
      proposedNextRunAt = case decision of
        MoveJobTo _ newAt -> newAt
        LeaveInsideBuffer queuedAt -> queuedAt
        NotMoved (Just queuedAt) -> queuedAt
        _ -> scheduledFromProposed
  let effect =
        ConfigScheduleEffect
          { currentNextRunAt = currentNextRunAt,
            proposedNextRunAt = proposedNextRunAt,
            decision = decision,
            bufferMinutes = proposed.rescheduleBufferMinutes
          }
  pure (proposed, configDiff existing proposed, effect)

resolveOpCityId :: Id.ShortId DM.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.Flow Text
resolveOpCityId merchantShortId opCity = do
  merchant <- QM.findByShortId merchantShortId >>= fromMaybeM (MerchantDoesNotExist merchantShortId.getShortId)
  merchantOpCity <- CQMOC.findByMerchantIdAndCity merchant.id opCity >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchant-Id-" <> merchant.id.getId <> "-city-" <> show opCity)
  pure merchantOpCity.id.getId

-- | Field-by-field, over the fields an edit can touch. @updatedAt@ is skipped: it always differs and
--   says nothing about the edit.
configDiff :: DSPC.ScheduledPayoutConfig -> DSPC.ScheduledPayoutConfig -> [ConfigFieldDiff]
configDiff old new =
  catMaybes
    [ cmp "isEnabled" (show . (.isEnabled)),
      cmp "frequency" (show . (.frequency)),
      cmp "dayOfWeek" (showMb . (.dayOfWeek)),
      cmp "dayOfMonth" (showMb . (.dayOfMonth)),
      cmp "timeOfDay" (.timeOfDay),
      cmp "batchSize" (show . (.batchSize)),
      cmp "minimumPayoutAmount" (show . (.minimumPayoutAmount)),
      cmp "maxRetriesPerDriver" (show . (.maxRetriesPerDriver)),
      cmp "vehicleCategory" (showMb . (.vehicleCategory)),
      cmp "remark" (fromMaybe "" . (.remark)),
      cmp "orderType" (.orderType),
      cmp "timeDiffFromUtc" (show . (.timeDiffFromUtc)),
      cmp "intervalHours" (showMb . (.intervalHours)),
      cmp "intervalDays" (showMb . (.intervalDays)),
      cmp "rescheduleBufferMinutes" (showMb . (.rescheduleBufferMinutes)),
      cmp "bufferCheckEnabled" (showMb . (.bufferCheckEnabled)),
      cmp "itemsPerBatchLimit" (showMb . (.itemsPerBatchLimit)),
      cmp "defaultPayoutRail" (showMb . (.defaultPayoutRail)),
      cmp "bulkStatusCheckIntervalMinutes" (showMb . (.bulkStatusCheckIntervalMinutes)),
      cmp "bulkStatusCheckBatchLimit" (showMb . (.bulkStatusCheckBatchLimit))
    ]
  where
    showMb :: Show a => Maybe a -> Text
    showMb = maybe "" show
    cmp name render =
      let before = render old
          after = render new
       in if before == after
            then Nothing
            else Just (ConfigFieldDiff {field = name, from = nonEmpty' before, to = nonEmpty' after})
    nonEmpty' t = if T.null t then Nothing else Just t

-- NOTE (reviewer, remove before merge): Main's upsert never clears this cache, so the next edit can read (and write
--   back) the config from before the previous edit. Shared with Juspay/Stripe, on purpose: their next read just sees the
--   saved row sooner. Clears the in-memory copy too, the way the transporter-config edits do.

-- | getOneConfig serves ScheduledPayoutConfig from a cache that a save here does not otherwise
--   clear, so without this the next edit reads (and writes back) the config from before this one.
--   Clears the Redis bucket and every pod's in-memory copy.
clearScheduledPayoutConfigCache :: Environment.Flow ()
clearScheduledPayoutConfigCache = invalidateConfigInMem CP.ScheduledPayoutConfig

-- NOTE (reviewer, remove before merge): New validation; main saves any value. Shared with Juspay/Stripe, on purpose. Rules:
--   * HOURLY needs intervalHours 1..23 and EVERY_N_DAYS needs intervalDays >= 2 (the two new frequencies);
--   * WEEKLY needs dayOfWeek 1 (Monday) .. 7 (Sunday); main accepts any number and treats a missing day as Monday;
--   * MONTHLY needs dayOfMonth 1..28 (main silently runs 29-31 on the 28th);
--   * timeOfDay must be exactly HH:MM, except for HOURLY (main accepts "2:00");
--   * batchSize >= 1, and rescheduleBufferMinutes >= 15 when bufferCheckEnabled is true;
--   * bulkStatusCheckIntervalMinutes >= 1 and bulkStatusCheckBatchLimit in 1..500 when set.
--   Before deploying, check production for rows that would fail (e.g. WEEKLY with a missing or wrong dayOfWeek), because
--   every later edit of such a row would be refused.

-- | Validate a config before it is saved.
--   Checked on the merged config, by the create, the update and the VIEW_DIFF preview alike, so a
--   preview raises exactly the error the save would. minimumPayoutAmount is deliberately not checked.
validateScheduledPayoutConfig :: DSPC.ScheduledPayoutConfig -> Environment.Flow ()
validateScheduledPayoutConfig cfg = do
  case cfg.frequency of
    DSPC.HOURLY -> case cfg.intervalHours of
      Just h | h >= 1 && h <= 23 -> pure ()
      _ -> throwError $ InvalidRequest "HOURLY frequency requires intervalHours in 1..23"
    DSPC.EVERY_N_DAYS -> case cfg.intervalDays of
      Just d | d >= 2 -> pure ()
      _ -> throwError $ InvalidRequest "EVERY_N_DAYS frequency requires intervalDays >= 2"
    DSPC.WEEKLY -> case cfg.dayOfWeek of
      Just day | day >= 1 && day <= 7 -> pure ()
      _ -> throwError $ InvalidRequest "WEEKLY frequency requires dayOfWeek from 1 (Monday) to 7 (Sunday)"
    DSPC.MONTHLY -> case cfg.dayOfMonth of
      -- 29-31 would be silently clamped to 28 by computeNextRunTime, so they are refused instead.
      Just d | d >= 1 && d <= 28 -> pure ()
      _ -> throwError $ InvalidRequest "MONTHLY frequency requires dayOfMonth in 1..28"
    _ -> pure ()
  -- HOURLY ignores timeOfDay (next run = now + intervalHours); every other frequency fires at it, and
  -- an unparseable value would otherwise fall back to 02:00 without a word.
  unless (cfg.frequency == DSPC.HOURLY || validTimeOfDay cfg.timeOfDay) $
    throwError $ InvalidRequest ("timeOfDay must be HH:MM (00:00-23:59), got: " <> cfg.timeOfDay)
  when (cfg.batchSize < 1) $
    throwError $ InvalidRequest "batchSize must be at least 1"
  -- Only meaningful when re-timing is on. Below 15 minutes the queued run is moved even when it is
  -- about to fire; there is no upper limit.
  when (cfg.bufferCheckEnabled == Just True) $
    case cfg.rescheduleBufferMinutes of
      Just m | m >= 15 -> pure ()
      _ -> throwError $ InvalidRequest "rescheduleBufferMinutes must be at least 15 when bufferCheckEnabled is true"
  when (maybe False (< 1) cfg.bulkStatusCheckIntervalMinutes) $
    throwError $ InvalidRequest "bulkStatusCheckIntervalMinutes must be at least 1"
  -- A run also stops starting calls after 30 s, so a limit above a few hundred would only read rows
  -- the run never gets to.
  when (maybe False (\n -> n < 1 || n > 500) cfg.bulkStatusCheckBatchLimit) $
    throwError $ InvalidRequest "bulkStatusCheckBatchLimit must be in 1..500"
  where
    validTimeOfDay t = case T.splitOn ":" t of
      [hh, mm] | T.length hh == 2 && T.length mm == 2 ->
        case (readMaybe (T.unpack hh) :: Maybe Int, readMaybe (T.unpack mm) :: Maybe Int) of
          (Just h, Just m) -> h >= 0 && h <= 23 && m >= 0 && m <= 59
          _ -> False
      _ -> False

-- | Turning the schedule back on (or creating it enabled): put the sweep job at the new schedule's
--   next run -- the waiting job is moved, or a job is created. See 'resumeSweepJob'.
resumeScheduledBatchPayoutJob :: DSPC.ScheduledPayoutConfig -> Environment.Flow ()
resumeScheduledBatchPayoutJob = resumeSweepJob
