module SharedLogic.Finance.WalletPayout
  ( PayoutContext (..),
    WalletPayoutParams (..),
    loadPayoutContext,
    ensurePayoutsEnabled,
    computePayoutFee,
    runWalletPayout,
  )
where

import Domain.Action.UI.Ride.EndRide.Internal (makeWalletRunningBalanceLockKey)
import Domain.Types.Extra.Plan
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.MerchantServiceConfig as DEMSC
import qualified Domain.Types.Person as DP
import qualified Domain.Types.TransporterConfig as DTConf
import Kernel.External.Encryption (decrypt)
import qualified Kernel.External.Notification.FCM.Types as FCM
import qualified Kernel.External.Payout.Interface as IPayout
import Kernel.External.Types (SchedulerFlow, ServiceFlow)
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.ConfigPilot.Interface.Types (getOneConfig)
import Lib.Finance (Account, EntryType (Reversal), LedgerEntryMetadata (..), findByAccountWithFilters)
import qualified Lib.Finance.Core.Types as Finance
import Lib.Finance.Storage.Beam.BeamFlow (BeamFlow)
import qualified Lib.Payment.Domain.Types.Common as DPayment
import qualified Lib.Payment.Domain.Types.PayoutRequest as PR
import qualified Lib.Payment.Payout.Request as PayoutRequest
import SharedLogic.Finance.Wallet
import qualified SharedLogic.Payment as SPayment
import SharedLogic.PayoutStatusCheck (afterPayoutOrderCreated)
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import Storage.ConfigPilot.Config.TransporterConfig (TransporterConfigDimensions (..))
import qualified Storage.Queries.DriverInformation as QDI
import qualified Storage.Queries.FleetOwnerInformationExtra as QFOI
import qualified Storage.Queries.Person as QPerson
import Tools.Error
import qualified Tools.Notifications as Notify
import qualified Tools.Payout as Payout

data PayoutContext = PayoutContext
  { driverId :: Id DP.Person,
    merchantId :: Id Domain.Types.Merchant.Merchant,
    mocId :: Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
    person :: DP.Person,
    payoutVpa :: Maybe Text,
    transporterConfig :: DTConf.TransporterConfig
  }

data WalletPayoutParams = WalletPayoutParams
  { payoutType :: PR.PayoutType,
    minimumPayoutAmount :: HighPrecMoney,
    enforceDailyLimit :: Bool,
    throwOnBelowMinimum :: Bool
  }

data WalletPayoutPlan = WalletPayoutPlan
  { payoutableBalance :: HighPrecMoney,
    cutoff :: UTCTime,
    redeemableEntryIds :: [Text],
    merchantTransferAmount :: HighPrecMoney
  }

type WalletPayoutFlow m r =
  ( EncFlow m r,
    CacheFlow m r,
    Finance.HasActorInfo m r,
    EsqDBFlow m r,
    BeamFlow m r,
    ServiceFlow m r,
    HasFlowEnv m r '["selfBaseUrl" ::: BaseUrl],
    Redis.HedisLTSFlowEnv r,
    SchedulerFlow r,
    HasField "blackListedJobs" r [Text]
  )

loadPayoutContext ::
  (EsqDBFlow m r, CacheFlow m r) =>
  Maybe (Id DP.Person) ->
  Id Domain.Types.Merchant.Merchant ->
  Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity ->
  m PayoutContext
loadPayoutContext mbPersonId merchantId mocId = do
  driverId <- fromMaybeM (PersonDoesNotExist "Nothing") mbPersonId
  transporterConfig <- getOneConfig (TransporterConfigDimensions {merchantOperatingCityId = mocId.getId}) Nothing >>= fromMaybeM (TransporterConfigNotFound mocId.getId)
  person <- QPerson.findById driverId >>= fromMaybeM (PersonNotFound driverId.getId)
  payoutVpa <- case person.role of
    DP.FLEET_OWNER -> do
      mbFleetInfo <- QFOI.findByPersonIdAndEnabledAndVerified Nothing Nothing driverId
      pure (mbFleetInfo >>= (.payoutVpa))
    DP.FLEET_BUSINESS -> do
      mbFleetInfo <- QFOI.findByPersonIdAndEnabledAndVerified Nothing Nothing driverId
      pure (mbFleetInfo >>= (.payoutVpa))
    _ -> do
      mbDriverInfo <- QDI.findById driverId
      pure (mbDriverInfo >>= (.payoutVpa))
  pure PayoutContext {..}

ensurePayoutsEnabled :: (MonadFlow m) => PayoutContext -> m ()
ensurePayoutsEnabled ctx =
  unless ctx.transporterConfig.driverWalletConfig.enableWalletPayout $
    throwError $ InvalidRequest "Payouts are disabled"

ensurePayoutLimitNotReached ::
  (BeamFlow m r) =>
  PayoutContext ->
  Maybe (Id Account) ->
  UTCTime ->
  m ()
ensurePayoutLimitNotReached ctx mbAccountId now = do
  let timeDiff = secondsToNominalDiffTime ctx.transporterConfig.timeDiffFromUtc
      (utcStartOfDay, utcEndOfDay) = todayRangeUTC timeDiff now
  whenJust ctx.transporterConfig.driverWalletConfig.maxWalletPayoutsPerDay $ \maxPayoutsPerDay -> do
    payoutsToday <- case mbAccountId of
      Nothing -> pure []
      Just accountId -> filter ((/= Reversal) . (.entryType)) <$> findByAccountWithFilters accountId (Just utcStartOfDay) (Just utcEndOfDay) Nothing Nothing Nothing (Just [walletReferencePayout])
    when (length payoutsToday >= maxPayoutsPerDay) $
      throwError $ InvalidRequest "Maximum payouts per day reached"

resolvePayoutVpa :: (MonadFlow m) => PayoutContext -> m Text
resolvePayoutVpa ctx =
  fromMaybeM (InternalError $ "Payout vpa not present for " <> ctx.driverId.getId) ctx.payoutVpa

computePayoutFee :: Maybe DTConf.PayoutFeeConfig -> HighPrecMoney -> HighPrecMoney
computePayoutFee Nothing _ = 0
computePayoutFee (Just feeConfig) amount =
  min amount . SPayment.roundToTwoDecimalPlaces $ computeStripePayoutFee feeConfig amount

-- | One wallet payout attempt for a payee under the wallet lock: the amount finder and the
--   initiate (payout request + OwnerPayoutLiability hold) both run inside it.
runWalletPayout :: (WalletPayoutFlow m r) => PayoutContext -> WalletPayoutParams -> m ()
runWalletPayout ctx params =
  PayoutRequest.runPayoutUnderLock (makeWalletRunningBalanceLockKey ctx.driverId.getId) 10 (findWalletPayoutAmount ctx params) (initiateWalletPayout ctx params.payoutType)

findWalletPayoutAmount :: (WalletPayoutFlow m r) => PayoutContext -> WalletPayoutParams -> m (Maybe WalletPayoutPlan)
findWalletPayoutAmount ctx params = do
  now <- getCurrentTime
  let counterparty = counterpartyFromRole ctx.person.role
  mbAccount <- getWalletAccountByOwner counterparty ctx.driverId.getId
  let mbAccountId = (.id) <$> mbAccount
  walletBalance <- fromMaybe 0 <$> getWalletBalanceByOwner counterparty ctx.driverId.getId
  when params.enforceDailyLimit $ ensurePayoutLimitNotReached ctx mbAccountId now
  let timeDiff = secondsToNominalDiffTime ctx.transporterConfig.timeDiffFromUtc
      cutOffDays = ctx.transporterConfig.driverWalletConfig.payoutCutOffDays
      cutoff = payoutCutoffTimeUTC timeDiff cutOffDays now
  eligibility <- case mbAccountId of
    Nothing -> pure emptyWalletPayoutEligibility
    Just accountId -> getPayoutEligibilityData counterparty ctx.driverId.getId accountId walletBalance cutoff now
  let payoutableBalance = eligibility.redeemableBalance
  logInfo $ "Payout eligibility for " <> ctx.driverId.getId <> ": walletBalance=" <> show walletBalance <> ", nonRedeemable=" <> show eligibility.nonRedeemableBalance <> ", processingPayout=" <> show eligibility.processingPayoutBalance <> ", payoutableBalance=" <> show payoutableBalance <> ", minimum=" <> show params.minimumPayoutAmount <> ", redeemableEntryIds=" <> show eligibility.redeemableEntryIds
  if payoutableBalance < params.minimumPayoutAmount
    then do
      when params.throwOnBelowMinimum $ throwError $ InvalidRequest ("Minimum payout amount is " <> show params.minimumPayoutAmount)
      pure Nothing
    else
      pure $
        Just
          WalletPayoutPlan
            { payoutableBalance,
              cutoff,
              redeemableEntryIds = map (.getId) eligibility.redeemableEntryIds,
              merchantTransferAmount = eligibility.merchantTransferAmount
            }

initiateWalletPayout :: (WalletPayoutFlow m r) => PayoutContext -> PR.PayoutType -> WalletPayoutPlan -> m ()
initiateWalletPayout ctx payoutType WalletPayoutPlan {..} = do
  phoneNo <- mapM decrypt ctx.person.mobileNumber
  merchantOperatingCity <- CQMOC.findById (cast ctx.person.merchantOperatingCityId) >>= fromMaybeM (MerchantOperatingCityNotFound ctx.person.merchantOperatingCityId.getId)
  (payoutServiceFlow, payoutServiceName, mbPersonBankAccount) <- Payout.getCreatePayoutServiceFlow (Payout.SubscriptionConfigOption PREPAID_SUBSCRIPTION) DEMSC.PayoutService ctx.person.clientSdkVersion ctx.person.merchantOperatingCityId ctx.person.id
  vpa <- case payoutServiceFlow of
    IPayout.JuspayFlow -> Just <$> resolvePayoutVpa ctx
    IPayout.StripeFlow -> pure Nothing
  let fee = computePayoutFee ctx.transporterConfig.driverWalletConfig.payoutFee payoutableBalance
      -- Floor (not round-half-up) to whole cents so the disbursed amount never exceeds the
      -- wallet balance; otherwise the hold can overdraw the wallet by a sub-cent rounding delta.
      netAmount = SPayment.floorToTwoDecimalPlaces (payoutableBalance - fee)
      submission =
        PayoutRequest.PayoutSubmission
          { beneficiaryId = ctx.driverId.getId,
            entityName = DPayment.DRIVER_WALLET_TRANSACTION,
            entityId = ctx.driverId.getId,
            entityRefId = Nothing,
            amount = netAmount,
            currency = ctx.transporterConfig.currency,
            payoutFee = if fee > 0 then Just fee else Nothing,
            transferAmount = Just merchantTransferAmount,
            merchantId = ctx.merchantId.getId,
            merchantOpCityId = ctx.mocId.getId,
            city = show merchantOperatingCity.city,
            vpa = vpa,
            -- mbPersonBankAccount is the account the payout is actually sent to, which for a
            -- fleet driver is the fleet owner's rather than their own.
            bankName = mbPersonBankAccount >>= (.bankName),
            bankAccountLast4 = mbPersonBankAccount >>= (.bankAccountLast4),
            customerName = Just ctx.person.firstName,
            customerPhone = phoneNo,
            customerEmail = ctx.person.email,
            remark = "Settlement for wallet",
            orderType = "FULFILL_ONLY",
            scheduledAt = Nothing,
            payoutType = Just payoutType,
            coverageFrom = Nothing,
            coverageTo = Just cutoff,
            ledgerEntryIds = [],
            payoutServiceFlow
          }
      payoutCall = Payout.createPayoutOrder payoutServiceName ctx.person.merchantOperatingCityId ctx.person.id mbPersonBankAccount

  when (netAmount > 0.0) $ do
    result <- PayoutRequest.submitPayoutRequest submission payoutCall afterPayoutOrderCreated
    case result of
      PayoutRequest.PayoutInitiated pr _ -> do
        let counterparty = counterpartyFromRole ctx.person.role
            ownerPayoutCtx = buildDriverChargeCtx counterparty ctx.driverId.getId ctx.merchantId.getId ctx.mocId.getId ctx.transporterConfig.currency pr.id.getId (fromMaybe False ctx.transporterConfig.driverWalletConfig.enableWalletGatedTierCheck)
            metadata =
              LedgerEntryMetadata
                { driverPayable = Just (negate netAmount),
                  payoutOrderId = Nothing,
                  reason = Nothing,
                  subscriptionAllocations = Nothing,
                  d2cReferralEarnings = Nothing,
                  d2dReferralEarnings = Nothing,
                  dailyStatsId = Nothing
                }
        postOwnerPayoutLiability ownerPayoutCtx netAmount (Just metadata)
          >>= either (\err -> logError $ "Failed to move payout amount to owner payout liability for payoutRequest " <> pr.id.getId <> ": " <> show err) (const (pure ()))
        PayoutRequest.stashPayoutLedgerEntryIds pr.id.getId redeemableEntryIds
        Notify.sendNotificationToDriver ctx.person.merchantOperatingCityId FCM.SHOW Nothing FCM.PAYOUT_INITIATED "Payout Initiated" ("Your payout of " <> show netAmount <> " has been initiated." <> if fee > 0 then " (Fee: " <> show fee <> ")" else "") ctx.person ctx.person.deviceToken
      PayoutRequest.PayoutProcessing pr status ->
        logInfo $ "Wallet payout already in flight for " <> ctx.driverId.getId <> " | payoutRequestId: " <> pr.id.getId <> " | status: " <> show status
      PayoutRequest.PayoutFailed _ err ->
        logError $ "Wallet payout failed for " <> ctx.driverId.getId <> ": " <> err
