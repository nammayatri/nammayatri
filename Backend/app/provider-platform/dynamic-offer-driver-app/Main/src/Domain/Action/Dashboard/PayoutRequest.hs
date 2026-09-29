{-# OPTIONS_GHC -Wno-orphans #-}

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
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.APISuccess (APISuccess (Success))
import qualified Kernel.Types.Beckn.Context
import Kernel.Types.Error
import qualified Kernel.Types.Id as Id
import Kernel.Utils.Common
import Lib.ConfigPilot.Interface.Types (ConfigDimensions (getConfig), getOneConfig)
import Lib.Finance.Storage.Beam.BeamFlow (BeamFlow)
import Lib.Payment.API.Payout (VerifyVpaFlow (..))
import qualified Lib.Payment.API.Payout.Types as PayoutTypes
import qualified Lib.Payment.Domain.Action as Payout
import qualified Lib.Payment.Domain.Types.Common as DPayment
import qualified Lib.Payment.Domain.Types.PayoutBatch as DPayoutBatch
import qualified Lib.Payment.Payout.Registration as Registration
import Lib.Scheduler.JobStorageType.SchedulerType (createJobInWithCheck)
import qualified Lib.Yudhishthira.Types.ConfigPilot as CP
import SharedLogic.Allocator (AllocatorJobType (..), ScheduledBatchPayoutJobData (..))
import SharedLogic.Allocator.Jobs.Payout.ScheduledBatchPayout (computeNextRunTime)
import SharedLogic.Payout.RetimeScheduledBatchPayout (RetimeDecision (..), previewRetime, retimeQueuedPayoutJob, scheduleChanged)
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

  -- Eligibility check: VPA must be captured via webhook, refund not already done
  let payoutVpaValid = case payoutServiceFlow of
        IPayout.JuspayFlow -> driverInfo.payoutVpaStatus == Just DI.VIA_WEBHOOK && isJust driverInfo.payoutVpa
        IPayout.StripeFlow -> True
        IPayout.BulkFlow -> isJust mbPersonBankAccount
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
    defaultPayoutRail :: Maybe DPayoutBatch.PayoutBatchRail
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

-- | Whether committing @updated@ over @existing@ moves the already-queued job. The single rule for
--   both the commit and the VIEW_DIFF preview, so the preview cannot promise a move the commit skips.
--   A resume is excluded because it creates its own job instead.
willRetimeOnCommit :: DSPC.ScheduledPayoutConfig -> DSPC.ScheduledPayoutConfig -> Bool
willRetimeOnCommit existing updated =
  updated.bufferCheckEnabled == Just True && updated.isEnabled && existing.isEnabled && scheduleChanged existing updated

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
      QSPC.updateByPrimaryKey updated
      clearScheduledPayoutConfigCache
      when isResuming $
        resumeScheduledBatchPayoutJob updated
      -- Move the already-queued job to the new schedule (only when bufferCheckEnabled; otherwise the
      -- edit applies after the queued run, as before). A failure leaves the old timing, not an error.
      when (willRetimeOnCommit existing updated) $ do
        res <- withTryCatch "retimeQueuedPayoutJob" $ retimeQueuedPayoutJob existing updated
        whenLeft res $ \err -> logWarning $ "ScheduledBatchPayout re-time failed, job keeps its old timing: " <> show err
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
                DSPC.createdAt = now,
                DSPC.updatedAt = now
              }
      validateScheduledPayoutConfig newConfig
      QSPC.create newConfig
      clearScheduledPayoutConfigCache
      when newConfig.isEnabled $
        resumeScheduledBatchPayoutJob newConfig
      logInfo $ "Created ScheduledPayoutConfig for " <> show req.payoutCategory <> " in city " <> merchantOpCity.id.getId
  pure Success

-- | The one place a dashboard request is merged onto a stored config. Shared by the commit above and
--   by the VIEW_DIFF preview below: a preview that could merge differently from the upsert would be
--   worse than no preview.
--
--   Present fields win, absent fields keep what is stored ('<|>' for the optional ones, 'fromMaybe'
--   for the mandatory), exactly as before this was given a name.
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
  Environment.Flow (DSPC.ScheduledPayoutConfig, DSPC.ScheduledPayoutConfig, [ConfigFieldDiff], ConfigScheduleEffect)
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
  let mbQueuedAt = case lookedUp of
        NoQueuedJob -> Nothing
        LeaveInsideBuffer queuedAt -> Just queuedAt
        MoveJobTo queuedAt _ -> Just queuedAt
        NotMoved mbAt -> mbAt
      decision = if willRetimeOnCommit existing proposed then lookedUp else NotMoved mbQueuedAt
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
  pure (existing, proposed, configDiff existing proposed, effect)

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
      cmp "defaultPayoutRail" (showMb . (.defaultPayoutRail))
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

-- | getOneConfig serves ScheduledPayoutConfig from a Redis cache that nothing else clears, so without
--   this the next edit reads (and writes back) the config from before this one. In-mem cache is off.
clearScheduledPayoutConfigCache :: Environment.Flow ()
clearScheduledPayoutConfigCache = Redis.delRedisCacheBucket ("ConfigPilot:" <> show CP.ScheduledPayoutConfig)

-- | Validate frequency-specific interval params (HOURLY / EVERY_N_DAYS) before persisting.
-- | Checked on the merged config, by the create, the update and the VIEW_DIFF preview alike, so a
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
  where
    validTimeOfDay t = case T.splitOn ":" t of
      [hh, mm] | T.length hh == 2 && T.length mm == 2 ->
        case (readMaybe (T.unpack hh) :: Maybe Int, readMaybe (T.unpack mm) :: Maybe Int) of
          (Just h, Just m) -> h >= 0 && h <= 23 && m >= 0 && m <= 59
          _ -> False
      _ -> False

-- | (Re)creates the ScheduledBatchPayout job on enable/resume, so it no longer needs a manual
--   dashboard trigger. First tick is scheduled via computeNextRunTime, not immediately.
resumeScheduledBatchPayoutJob :: DSPC.ScheduledPayoutConfig -> Environment.Flow ()
resumeScheduledBatchPayoutJob config = do
  nextRunTime <- computeNextRunTime config
  now <- getCurrentTime
  let inTime = diffUTCTime nextRunTime now
      jobData =
        ScheduledBatchPayoutJobData
          { merchantId = config.merchantId,
            merchantOperatingCityId = config.merchantOperatingCityId,
            payoutCategory = config.payoutCategory,
            vehicleCategory = config.vehicleCategory
          }
  Redis.runInMasterCloudRedisCell $
    createJobInWithCheck @_ @'ScheduledBatchPayout
      (Just config.merchantId)
      (Just config.merchantOperatingCityId)
      inTime
      (addUTCTime (-86400) now)
      (addUTCTime (40 * 86400) now)
      "ScheduledBatchPayout"
      (Just 1)
      jobData
  logInfo $ "Resumed/created ScheduledBatchPayout job for " <> show config.payoutCategory <> " in city " <> config.merchantOperatingCityId.getId <> ", next run at " <> show nextRunTime
