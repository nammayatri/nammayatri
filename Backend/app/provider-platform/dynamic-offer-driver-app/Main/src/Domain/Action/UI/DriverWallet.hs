{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.UI.DriverWallet (getWalletTransactions, postWalletPayout, postWalletTopup, getWalletPayoutHistory) where

import qualified API.Types.UI.DriverWallet as DriverWallet
import Data.List (partition)
import qualified Data.Map.Strict as Map
import qualified Data.Time
import Domain.Action.UI.Plan hiding (mkDriverFee)
import Domain.Action.UI.Ride.EndRide.Internal (makeWalletRunningBalanceLockKey)
import qualified Domain.Types.DriverFee as DF
import qualified Domain.Types.DriverInformation as DDI
import Domain.Types.Extra.Plan
import qualified Domain.Types.Invoice as INV
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.MerchantServiceConfig as DEMSC
import qualified Domain.Types.Person as DP
import qualified Domain.Types.TransporterConfig as DTConf
import Domain.Types.VehicleCategory
import qualified Environment
import EulerHS.Prelude hiding (id)
import Kernel.External.Encryption (decrypt)
import qualified Kernel.External.Notification.FCM.Types as FCM
import qualified Kernel.External.Payment.Types as Payment
import qualified Kernel.External.Payout.Types as TPayout
import qualified Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import qualified Kernel.Types.APISuccess as APISuccess
import Kernel.Types.Id (Id (..))
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Lib.Finance
import Lib.Finance.Storage.Beam.BeamFlow (BeamFlow)
import qualified Lib.Payment.Domain.Types.Common as DPayment
import qualified Lib.Payment.Domain.Types.PayoutRequest as PR
import qualified Lib.Payment.Payout.PayoutItems as PayoutItems
import qualified Lib.Payment.Payout.Request as PayoutRequest
import SharedLogic.Finance.Prepaid (counterpartyDriver, counterpartyFleetOwner)
import SharedLogic.Finance.Wallet
import qualified SharedLogic.Payment as SPayment
import Storage.Cac.TransporterConfig (findByMerchantOpCityId)
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.CachedQueries.SubscriptionConfig as CQSC
import qualified Storage.Queries.DriverFee as QDF
import qualified Storage.Queries.DriverFeeExtra as QDFE
import qualified Storage.Queries.DriverInformation as QDI
import qualified Storage.Queries.Person as QPerson
import Tools.Error
import qualified Tools.Notifications as Notify
import qualified Tools.Payout as Payout

-- | Pick the counterparty type based on the person's role.
counterpartyFromRole :: DP.Role -> CounterpartyType
counterpartyFromRole DP.FLEET_OWNER = counterpartyFleetOwner
counterpartyFromRole DP.FLEET_BUSINESS = counterpartyFleetOwner
counterpartyFromRole _ = counterpartyDriver

--------------------------------------------------------------------------------
-- getWalletTransactions
--------------------------------------------------------------------------------

getWalletTransactions ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id DP.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Kernel.Prelude.Maybe Data.Time.UTCTime ->
    Kernel.Prelude.Maybe Data.Time.UTCTime ->
    Environment.Flow DriverWallet.WalletSummaryResponse
  )
getWalletTransactions (mbPersonId, _merchantId, mocId) mbFromDate mbToDate = do
  driverId <- fromMaybeM (PersonDoesNotExist "Nothing") mbPersonId
  person <- QPerson.findById driverId >>= fromMaybeM (PersonNotFound driverId.getId)
  let counterparty = counterpartyFromRole person.role
  transporterConfig <- findByMerchantOpCityId mocId Nothing >>= fromMaybeM (TransporterConfigNotFound mocId.getId)
  now <- getCurrentTime
  let timeDiff = secondsToNominalDiffTime transporterConfig.timeDiffFromUtc
      fromDate = fromMaybe (Data.Time.UTCTime (Data.Time.utctDay now) 0) mbFromDate
      toDate = fromMaybe now mbToDate
  mbAccount <- getWalletAccountByOwner counterparty driverId.getId
  case mbAccount of
    Nothing -> pure emptyWalletSummary
    Just account -> do
      currentBalance <- fromMaybe 0 <$> getWalletBalanceByOwner counterparty driverId.getId
      nonRedeemableBalance <- getNonRedeemableBalance account.id timeDiff transporterConfig.driverWalletConfig.payoutCutOffDays now
      let redeemableBalance = max 0 (currentBalance - nonRedeemableBalance)
      (additions, deductions) <- classifyEntries account.id fromDate toDate account.id
      pure $
        DriverWallet.WalletSummaryResponse
          { currentBalance,
            redeemableBalance,
            nonRedeemableBalance,
            additions,
            deductions
          }

emptyWalletSummary :: DriverWallet.WalletSummaryResponse
emptyWalletSummary =
  DriverWallet.WalletSummaryResponse
    { currentBalance = 0,
      redeemableBalance = 0,
      nonRedeemableBalance = 0,
      additions = DriverWallet.WalletItemGroup {totalAmount = 0, items = []},
      deductions = DriverWallet.WalletItemGroup {totalAmount = 0, items = []}
    }

-- | Fetch entries in a date range, partition into additions/deductions,
--   and group by reference type.
classifyEntries ::
  (BeamFlow m r) =>
  Id Account ->
  UTCTime ->
  UTCTime ->
  Id Account -> -- owner account id (for addition/deduction classification)
  m (DriverWallet.WalletItemGroup, DriverWallet.WalletItemGroup)
classifyEntries accountId fromDate toDate ownerAccountId = do
  let allRefs =
        [ walletReferenceBaseRide,
          walletReferenceGSTOnline,
          walletReferenceGSTCash,
          walletReferenceTollCharges,
          walletReferenceParkingCharges,
          walletReferenceTDSDeductionOnline,
          walletReferenceTDSDeductionCash,
          walletReferenceTopup,
          walletReferencePayout
        ]
  entries <- findByAccountWithFilters accountId (Just fromDate) (Just toDate) Nothing Nothing Nothing (Just allRefs)
  let (addEntries, dedEntries) = partition (\e -> e.toAccountId == ownerAccountId) entries
      toGroup es =
        let refMap = foldl' (\acc e -> Map.insertWith (+) e.referenceType e.amount acc) Map.empty es
            items = map (\(ref, val) -> DriverWallet.WalletItem {itemReference = ref, itemName = referenceTypeToItemName ref, itemValue = val}) (Map.toList refMap)
         in DriverWallet.WalletItemGroup {totalAmount = sum (Map.elems refMap), items}
  pure (toGroup addEntries, toGroup dedEntries)

referenceTypeToItemName :: Text -> Text
referenceTypeToItemName ref
  | ref == walletReferenceBaseRide = "Ride Earnings"
  | ref == walletReferenceTollCharges = "Toll Charges"
  | ref == walletReferenceParkingCharges = "Parking Charges"
  | ref == walletReferenceTopup = "Wallet Top-up"
  | ref == walletReferenceGSTOnline = "GST (Online)"
  | ref == walletReferenceGSTCash = "GST (Cash)"
  | ref == walletReferenceTDSDeductionOnline = "TDS (Online)"
  | ref == walletReferenceTDSDeductionCash = "TDS (Cash)"
  | ref == walletReferencePayout = "Withdrawal"
  | otherwise = ref

--------------------------------------------------------------------------------
-- getWalletPayoutHistory
--------------------------------------------------------------------------------

getWalletPayoutHistory ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id DP.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Kernel.Prelude.Maybe Data.Time.UTCTime ->
    Kernel.Prelude.Maybe Data.Time.UTCTime ->
    Kernel.Prelude.Maybe [PR.PayoutRequestStatus] ->
    Kernel.Prelude.Maybe Kernel.Prelude.Int ->
    Kernel.Prelude.Maybe Kernel.Prelude.Int ->
    Environment.Flow DriverWallet.PayoutHistoryResponse
  )
getWalletPayoutHistory (mbPersonId, _merchantId, _mocId) mbFrom mbTo mbStatuses mbLimit mbOffset = do
  driverId <- fromMaybeM (PersonDoesNotExist "Nothing") mbPersonId
  let statuses = fromMaybe [] mbStatuses
      limit = mbLimit
      offset = mbOffset

  items <- PayoutItems.getPayoutItems driverId.getId mbFrom mbTo statuses limit offset

  let apiItems = map toHistoryItem items
      totalPaidOut = sum [fromMaybe 0 i.amount | i <- items, isPaidOut i.status]
      totalPending = sum [fromMaybe 0 i.amount | i <- items, isPending i.status]
      lastPayout = Kernel.Prelude.listToMaybe [toHistoryItem i | i <- items, isPaidOut i.status]

  pure $
    DriverWallet.PayoutHistoryResponse
      { totalPaidOut,
        lastPayout,
        totalPending,
        items = apiItems
      }
  where
    isPaidOut s = s == PR.CREDITED || s == PR.CASH_PAID
    isPending s = s == PR.INITIATED || s == PR.PROCESSING

    toHistoryItem :: PayoutItems.PayoutItem -> DriverWallet.PayoutHistoryItem
    toHistoryItem i =
      DriverWallet.PayoutHistoryItem
        { amount = i.amount,
          payoutFee = i.payoutFee,
          entityName = i.entityName,
          status = i.status,
          timestamp = i.timestamp,
          payoutMethod = i.payoutMethod,
          payoutVpa = i.payoutVpa
        }

--------------------------------------------------------------------------------
-- postWalletPayout
--------------------------------------------------------------------------------

-- | All context needed for payout operations, loaded once at the start.
data PayoutContext = PayoutContext
  { driverId :: Kernel.Types.Id.Id DP.Person,
    merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
    mocId :: Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity,
    person :: DP.Person,
    driverInfo :: DDI.DriverInformation,
    transporterConfig :: DTConf.TransporterConfig
  }

loadPayoutContext ::
  ( EsqDBFlow m r,
    CacheFlow m r
  ) =>
  Kernel.Prelude.Maybe (Kernel.Types.Id.Id DP.Person) ->
  Kernel.Types.Id.Id Domain.Types.Merchant.Merchant ->
  Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity ->
  m PayoutContext
loadPayoutContext mbPersonId merchantId mocId = do
  driverId <- fromMaybeM (PersonDoesNotExist "Nothing") mbPersonId
  transporterConfig <- findByMerchantOpCityId mocId Nothing >>= fromMaybeM (TransporterConfigNotFound mocId.getId)
  person <- QPerson.findById driverId >>= fromMaybeM (PersonNotFound driverId.getId)
  driverInfo <- QDI.findById driverId >>= fromMaybeM DriverInfoNotFound
  pure PayoutContext {..}

-- | Validate that wallet payouts are enabled for this merchant.
ensurePayoutsEnabled :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => PayoutContext -> m ()
ensurePayoutsEnabled ctx = do
  merchant <- CQM.findById ctx.merchantId >>= fromMaybeM (MerchantNotFound ctx.merchantId.getId)
  let isPrepaidAndWalletEnabled = fromMaybe False merchant.prepaidSubscriptionAndWalletEnabled
  unless (isPrepaidAndWalletEnabled && ctx.transporterConfig.driverWalletConfig.enableWalletPayout) $
    throwError $ InvalidRequest "Payouts are disabled"

-- | Check that the driver hasn't exceeded the maximum daily payout count.
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
      Just accountId -> findByAccountWithFilters accountId (Just utcStartOfDay) (Just utcEndOfDay) Nothing Nothing Nothing (Just [walletReferencePayout])
    when (length payoutsToday >= maxPayoutsPerDay) $
      throwError $ InvalidRequest "Maximum payouts per day reached"

-- | Compute the payoutable balance (wallet balance minus non-redeemable recent earnings).
computePayoutableBalance ::
  (BeamFlow m r) =>
  PayoutContext ->
  Maybe (Id Account) ->
  HighPrecMoney -> -- wallet balance
  UTCTime ->
  m HighPrecMoney
computePayoutableBalance ctx mbAccountId walletBalance now = do
  let timeDiff = secondsToNominalDiffTime ctx.transporterConfig.timeDiffFromUtc
      cutOffDays = ctx.transporterConfig.driverWalletConfig.payoutCutOffDays
  nonRedeemable <- case mbAccountId of
    Nothing -> pure 0
    Just accountId -> getNonRedeemableBalance accountId timeDiff cutOffDays now
  pure $ walletBalance - nonRedeemable

-- | Validate that the payoutable balance meets the minimum threshold.
ensureMinimumPayoutAmount :: (MonadFlow m) => PayoutContext -> HighPrecMoney -> m ()
ensureMinimumPayoutAmount ctx payoutableBalance = do
  let minAmount = ctx.transporterConfig.driverWalletConfig.minimumWalletPayoutAmount
  when (payoutableBalance < minAmount) $
    throwError $ InvalidRequest ("Minimum payout amount is " <> show minAmount)

-- | Extract and validate the driver's payout VPA.
resolvePayoutVpa :: (MonadFlow m) => PayoutContext -> m Text
resolvePayoutVpa ctx =
  fromMaybeM (InternalError $ "Payout vpa not present for " <> ctx.driverId.getId) ctx.driverInfo.payoutVpa

postWalletPayout ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id DP.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Environment.Flow APISuccess.APISuccess
  )
postWalletPayout (mbPersonId, merchantId, mocId) = do
  ctx <- loadPayoutContext mbPersonId merchantId mocId
  ensurePayoutsEnabled ctx
  let counterparty = counterpartyFromRole ctx.person.role
  Redis.withWaitOnLockRedisWithExpiry (makeWalletRunningBalanceLockKey ctx.driverId.getId) 10 10 $ do
    now <- getCurrentTime
    mbAccount <- getWalletAccountByOwner counterparty ctx.driverId.getId
    let mbAccountId = (.id) <$> mbAccount
    walletBalance <- fromMaybe 0 <$> getWalletBalanceByOwner counterparty ctx.driverId.getId
    ensurePayoutLimitNotReached ctx mbAccountId now
    payoutableBalance <- computePayoutableBalance ctx mbAccountId walletBalance now
    ensureMinimumPayoutAmount ctx payoutableBalance
    vpa <- resolvePayoutVpa ctx
    initiateWalletPayout ctx vpa payoutableBalance
  pure APISuccess.Success

-- | Create a PayoutRequest (INITIATED), then call Juspay createPayoutService (→ PROCESSING).
--   No ledger entry here — that happens in the webhook handler on SUCCESS.
initiateWalletPayout ::
  PayoutContext ->
  Text -> -- VPA
  HighPrecMoney -> -- payoutable balance
  Environment.Flow ()
initiateWalletPayout ctx vpa payoutableBalance = do
  phoneNo <- mapM decrypt ctx.person.mobileNumber
  subscriptionConfig <-
    CQSC.findSubscriptionConfigsByMerchantOpCityIdAndServiceName ctx.person.merchantOperatingCityId Nothing PREPAID_SUBSCRIPTION
      >>= fromMaybeM (NoSubscriptionConfigForService ctx.person.merchantOperatingCityId.getId "PREPAID_SUBSCRIPTION")
  payoutServiceName <- Payout.decidePayoutService (fromMaybe (DEMSC.PayoutService TPayout.Juspay) subscriptionConfig.payoutServiceName) ctx.person.clientSdkVersion ctx.person.merchantOperatingCityId
  merchantOperatingCity <- CQMOC.findById (Kernel.Types.Id.cast ctx.person.merchantOperatingCityId) >>= fromMaybeM (MerchantOperatingCityNotFound ctx.person.merchantOperatingCityId.getId)

  let submission =
        PayoutRequest.PayoutSubmission
          { beneficiaryId = ctx.driverId.getId,
            entityName = DPayment.DRIVER_WALLET_TRANSACTION,
            entityId = ctx.driverId.getId,
            entityRefId = Nothing,
            amount = SPayment.roundToTwoDecimalPlaces payoutableBalance,
            merchantId = ctx.merchantId.getId,
            merchantOpCityId = ctx.mocId.getId,
            city = show merchantOperatingCity.city,
            vpa = vpa,
            customerName = Just ctx.person.firstName,
            customerPhone = phoneNo,
            customerEmail = ctx.person.email,
            remark = "Settlement for wallet",
            orderType = "FULFILL_ONLY",
            scheduledAt = Nothing
          }
      payoutCall = Payout.createPayoutOrder ctx.person.merchantId ctx.person.merchantOperatingCityId payoutServiceName (Just ctx.person.id.getId)

  when (payoutableBalance > 0.0) $ do
    result <- PayoutRequest.submitPayoutRequest submission payoutCall
    case result of
      PayoutRequest.PayoutInitiated _ _ ->
        Notify.sendNotificationToDriver ctx.person.merchantOperatingCityId FCM.SHOW Nothing FCM.PAYOUT_INITIATED "Payout Initiated" ("Your payout of " <> show payoutableBalance <> " has been initiated.") ctx.person ctx.person.deviceToken
      PayoutRequest.PayoutFailed _ err ->
        logError $ "Wallet payout failed for driver " <> ctx.driverId.getId <> ": " <> err

--------------------------------------------------------------------------------
-- postWalletTopup
--------------------------------------------------------------------------------

postWalletTopup ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id DP.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    DriverWallet.TopUpRequest ->
    Environment.Flow PlanSubscribeRes
  )
postWalletTopup (mbPersonId, merchantId, mocId) req = do
  driverId <- fromMaybeM (PersonDoesNotExist "Nothing") mbPersonId
  transporterConfig <- findByMerchantOpCityId mocId Nothing >>= fromMaybeM (TransporterConfigNotFound mocId.getId)
  merchant <- CQM.findById merchantId >>= fromMaybeM (MerchantNotFound merchantId.getId)
  let isPrepaidSubscriptionAndWalletEnabled = fromMaybe False merchant.prepaidSubscriptionAndWalletEnabled
  unless (isPrepaidSubscriptionAndWalletEnabled && transporterConfig.driverWalletConfig.enableWalletTopup) $ throwError $ InvalidRequest "Wallet topups are disabled"
  when (req.amount <= 0) $ throwError $ InvalidRequest "Top-up amount must be greater than zero"
  eitherResult <- Redis.whenWithLockRedisAndReturnValue (makeWalletTopupLockKey driverId.getId) 10 $ do
    existingTopUpFee <- QDFE.findLatestByFeeTypeAndStatusWithTotalEarnings DF.WALLET_TOPUP [DF.PAYMENT_PENDING] driverId req.amount
    case existingTopUpFee of
      Just fee -> pure fee
      Nothing -> do
        driverFee <- mkDriverFee driverId req.amount transporterConfig.currency
        QDF.create driverFee
        pure driverFee
  case eitherResult of
    Right fee -> createTopupOrder driverId [fee]
    Left _ -> throwError $ InternalError "Could not acquire lock for wallet topup."
  where
    makeWalletTopupLockKey :: Text -> Text
    makeWalletTopupLockKey dId = "wallet-topup-lock:" <> dId

    createTopupOrder driverId driverFees = do
      (createOrderResp, orderId) <-
        SPayment.createOrder
          (driverId, merchantId, mocId)
          (DEMSC.PaymentService Payment.Juspay)
          (driverFees, [])
          Nothing
          INV.WALLET_TOPUP_INVOICE
          Nothing
          []
          Nothing
          False
          (Just DPayment.DRIVER_WALLET_TOPUP)
      return $
        PlanSubscribeRes
          { orderId = orderId,
            orderResp = createOrderResp
          }

    mkDriverFee driverId amount currency = do
      now <- getCurrentTime
      feeId <- generateGUID
      pure $
        DF.DriverFee
          { id = feeId,
            driverId = driverId,
            merchantId = merchantId,
            merchantOperatingCityId = mocId,
            feeType = DF.WALLET_TOPUP,
            status = DF.PAYMENT_PENDING,
            currency,
            platformFee = DF.PlatformFee {fee = 0, cgst = 0, sgst = 0, currency},
            govtCharges = 0,
            specialZoneAmount = 0,
            totalEarnings = amount,
            numRides = 0,
            specialZoneRideCount = 0,
            startTime = now,
            endTime = now,
            payBy = now,
            createdAt = now,
            updatedAt = now,
            serviceName = PREPAID_SUBSCRIPTION,
            vehicleCategory = CAR,
            notificationRetryCount = 0,
            schedulerTryCount = 0,
            overlaySent = False,
            amountPaidByCoin = Nothing,
            autopayPaymentStage = Nothing,
            badDebtDeclarationDate = Nothing,
            badDebtRecoveryDate = Nothing,
            billNumber = Nothing,
            collectedAt = Nothing,
            collectedBy = Nothing,
            feeWithoutDiscount = Nothing,
            hasSibling = Nothing,
            offerId = Nothing,
            planId = Nothing,
            planMode = Nothing,
            planOfferTitle = Nothing,
            refundEntityId = Nothing,
            refundedAmount = Nothing,
            refundedAt = Nothing,
            refundedBy = Nothing,
            siblingFeeId = Nothing,
            splitOfDriverFeeId = Nothing,
            stageUpdatedAt = Nothing,
            validDays = Nothing,
            vehicleNumber = Nothing,
            cancellationPenaltyAmount = Nothing,
            addedToFeeId = Nothing,
            collectedAtVendorId = Nothing
          }
