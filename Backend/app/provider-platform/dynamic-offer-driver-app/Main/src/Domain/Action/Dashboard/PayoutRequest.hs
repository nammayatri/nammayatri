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
  when (payoutServiceFlow == IPayout.BulkFlow) $
    throwError $ InvalidRequest "Registration refund is not supported in a city that pays through a bulk payout partner"

  -- Eligibility check: VPA must be captured via webhook, refund not already done
  let payoutVpaValid = case payoutServiceFlow of
        IPayout.JuspayFlow -> driverInfo.payoutVpaStatus == Just DI.VIA_WEBHOOK && isJust driverInfo.payoutVpa
        IPayout.StripeFlow -> True
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

-- | Whether a commit moves the already-queued job. Shared with the VIEW_DIFF preview so the preview
--   cannot promise a move the commit skips.
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
      -- Job first: if moving it fails nothing is saved, so the config and the job never disagree.
      when isResuming $
        resumeScheduledBatchPayoutJob updated
      -- Without bufferCheckEnabled the edit only takes effect after the queued run.
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
      validateScheduledPayoutConfig newConfig
      when newConfig.isEnabled $
        resumeScheduledBatchPayoutJob newConfig
      QSPC.create newConfig
      clearScheduledPayoutConfigCache
      logInfo $ "Created ScheduledPayoutConfig for " <> show req.payoutCategory <> " in city " <> merchantOpCity.id.getId
  pure Success

-- | Merge a dashboard request onto a stored config; absent fields keep the stored value.
--   Shared by the commit and the VIEW_DIFF preview so both merge the same way.
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

-- | One field an edit would change. Values are Text because the caller only displays them.
data ConfigFieldDiff = ConfigFieldDiff
  { field :: Text,
    from :: Maybe Text,
    to :: Maybe Text
  }

-- | How an edit would change the sweep's next run.
data ConfigScheduleEffect = ConfigScheduleEffect
  { currentNextRunAt :: UTCTime,
    proposedNextRunAt :: UTCTime,
    decision :: RetimeDecision,
    bufferMinutes :: Maybe Int
  }

-- | Configs for a city, optionally for one payout category. Read through config-pilot, like the
--   sweep, so the screen shows exactly what the job uses.
viewScheduledPayoutConfigs ::
  Id.ShortId DM.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Maybe DPayment.EntityName ->
  Environment.Flow [DSPC.ScheduledPayoutConfig]
viewScheduledPayoutConfigs merchantShortId opCity mbPayoutCategory = do
  merchantOpCityId <- resolveOpCityId merchantShortId opCity
  getConfig (ScheduledPayoutConfigDimensions {merchantOperatingCityId = merchantOpCityId, isEnabled = Nothing, payoutCategory = mbPayoutCategory}) Nothing

-- | Preview an edit without saving: the merged config, the changed fields and the effect on the
--   queued job. Built from the same functions the commit uses, so the two cannot disagree.
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
  validateScheduledPayoutConfig proposed
  scheduledFromCurrent <- computeNextRunTime existing
  scheduledFromProposed <- computeNextRunTime proposed
  -- Look up the queued job even if the commit will not move it: it is what actually fires next.
  lookedUp <- previewRetime existing proposed
  -- Turning the schedule back on goes through 'resumeSweepJob', not the retime rule.
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
      -- A job left queued still fires at its old time; the new schedule applies only if it is moved or created.
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

-- | Fields an edit changed. @updatedAt@ is skipped because it always differs.
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

-- | getOneConfig caches this config, so without this the next edit reads (and writes back) the old
--   one. Clears Redis and every pod's in-memory copy.
clearScheduledPayoutConfigCache :: Environment.Flow ()
clearScheduledPayoutConfigCache = invalidateConfigInMem CP.ScheduledPayoutConfig

-- | Run on the merged config by create, update and the VIEW_DIFF preview, so a preview raises the
--   same error a save would. minimumPayoutAmount is deliberately not checked.
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
  -- HOURLY ignores timeOfDay. For the others a bad value would silently fall back to 02:00.
  unless (cfg.frequency == DSPC.HOURLY || validTimeOfDay cfg.timeOfDay) $
    throwError $ InvalidRequest ("timeOfDay must be HH:MM (00:00-23:59), got: " <> cfg.timeOfDay)
  when (cfg.batchSize < 1) $
    throwError $ InvalidRequest "batchSize must be at least 1"
  -- Under 15 minutes a run that is about to fire could still be moved.
  when (cfg.bufferCheckEnabled == Just True) $
    case cfg.rescheduleBufferMinutes of
      Just m | m >= 15 -> pure ()
      _ -> throwError $ InvalidRequest "rescheduleBufferMinutes must be at least 15 when bufferCheckEnabled is true"
  when (maybe False (< 1) cfg.bulkStatusCheckIntervalMinutes) $
    throwError $ InvalidRequest "bulkStatusCheckIntervalMinutes must be at least 1"
  -- A run stops starting calls after 30 s, so rows beyond a few hundred would never be reached.
  when (maybe False (\n -> n < 1 || n > 500) cfg.bulkStatusCheckBatchLimit) $
    throwError $ InvalidRequest "bulkStatusCheckBatchLimit must be in 1..500"
  where
    validTimeOfDay t = case T.splitOn ":" t of
      [hh, mm] | T.length hh == 2 && T.length mm == 2 ->
        case (readMaybe (T.unpack hh) :: Maybe Int, readMaybe (T.unpack mm) :: Maybe Int) of
          (Just h, Just m) -> h >= 0 && h <= 23 && m >= 0 && m <= 59
          _ -> False
      _ -> False

-- | On resume or an enabled create, move the waiting sweep job to the new schedule, or create one.
resumeScheduledBatchPayoutJob :: DSPC.ScheduledPayoutConfig -> Environment.Flow ()
resumeScheduledBatchPayoutJob = resumeSweepJob
