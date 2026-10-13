{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- | Collecting a subscription driver_fee from the driver wallet (wallet-subscription cities):
--   the nightly fee and, once, every pending due moved in at switch-over. The wallet may go
--   negative. Each fee is posted at most once (reference = driver fee id) and is marked paid
--   only after the ledger post, so a retry never debits twice and never marks an unpaid fee.
module SharedLogic.Finance.WalletSubscription
  ( WalletDebitKind (..),
    walletCollectedBy,
    walletReferenceSubscriptionFee,
    walletReferenceSubscriptionGST,
    walletReferenceSubscriptionPenalty,
    walletReferenceMigratedDues,
    walletReferenceMigratedDuesGST,
    walletReferenceMigratedDuesPenalty,
    walletReferenceAutopayRecharge,
    walletReferenceLatePaymentCredit,
    walletReferenceBehaviourPenalty,
    resolveWalletOwner,
    debitDriverFeeFromWallet,
    walletDebitIncomplete,
    debitBehaviourPenaltyFromWallet,
    creditWalletForPaidDriverFees,
  )
where

import qualified Domain.Types.DriverFee as DDF
import qualified Domain.Types.Invoice as INV
import qualified Domain.Types.Person as DP
import qualified Domain.Types.TransporterConfig as DTC
import qualified Kernel.External.Notification.FCM.Types as FCM
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.Finance hiding (runFinance)
import Lib.Finance.Storage.Beam.BeamFlow (BeamFlow)
import SharedLogic.Finance.PostActions (runFinance)
import SharedLogic.Finance.Wallet (anyPosted, buildDriverChargeCtx, getPostedEntries, makeWalletRunningBalanceLockKey)
import qualified Storage.Queries.DriverFee as QDF
import qualified Storage.Queries.Invoice as QINV
import Tools.Notifications (sendNotificationToDriver)

data WalletDebitKind = DailySubscription | MigratedDues
  deriving (Show, Eq)

-- | driver_fee.collected_by for fees paid from the wallet.
walletCollectedBy :: Text
walletCollectedBy = "WALLET"

walletReferenceSubscriptionFee, walletReferenceSubscriptionGST, walletReferenceSubscriptionPenalty :: Text
walletReferenceSubscriptionFee = "WalletSubscriptionFee"
walletReferenceSubscriptionGST = "WalletSubscriptionGST"
walletReferenceSubscriptionPenalty = "WalletSubscriptionPenalty"

walletReferenceMigratedDues, walletReferenceMigratedDuesGST, walletReferenceMigratedDuesPenalty :: Text
walletReferenceMigratedDues = "WalletMigratedDues"
walletReferenceMigratedDuesGST = "WalletMigratedDuesGST"
walletReferenceMigratedDuesPenalty = "WalletMigratedDuesPenalty"

walletReferenceAutopayRecharge, walletReferenceLatePaymentCredit :: Text
walletReferenceAutopayRecharge = "WalletAutopayRecharge"
walletReferenceLatePaymentCredit = "WalletLatePaymentCredit"

-- | A penalty charged straight to the wallet at the moment it is decided (behaviour engine CHARGE_FEE).
walletReferenceBehaviourPenalty :: Text
walletReferenceBehaviourPenalty = "WalletBehaviourPenalty"

referenceTypes :: WalletDebitKind -> (Text, Text, Text)
referenceTypes = \case
  DailySubscription -> (walletReferenceSubscriptionFee, walletReferenceSubscriptionGST, walletReferenceSubscriptionPenalty)
  MigratedDues -> (walletReferenceMigratedDues, walletReferenceMigratedDuesGST, walletReferenceMigratedDuesPenalty)

-- | The wallet the charges of this feature settle in: always the driver's own, fleet driver or not.
--   (Ride earnings, ride GST and cancellation postings keep following the ride's fleet owner, as they
--   did before this feature; the airport entry fee at ride start is posted to the driver.)
resolveWalletOwner :: (Applicative m) => Id DP.Person -> m (CounterpartyType, Text)
resolveWalletOwner driverId = pure (DRIVER, driverId.getId)

-- | The three amounts a fee is debited as: its platform fee, the GST on it (cgst + sgst + govt charges)
--   and any cancellation penalty folded onto it.
feeDebitAmounts :: DDF.DriverFee -> (HighPrecMoney, HighPrecMoney, HighPrecMoney)
feeDebitAmounts fee =
  ( fee.platformFee.fee,
    fee.platformFee.cgst + fee.platformFee.sgst + fee.govtCharges,
    fromMaybe 0 fee.cancellationPenaltyAmount
  )

-- | Which of a fee's three debit components have no ledger entry yet, under either debit kind (a fee
--   debited as a nightly fee must never be debited again as migrated dues, or the reverse). The three
--   transfers are separate postings, not one transaction, so a crash can leave some of them: each
--   component is judged on its own reference type, all from one read of the fee's entries.
missingDebitComponents ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r, BeamFlow m r) =>
  DDF.DriverFee ->
  m (Bool, Bool, Bool)
missingDebitComponents fee = do
  let (dailyFee, dailyGst, dailyPenalty) = referenceTypes DailySubscription
      (migratedFee, migratedGst, migratedPenalty) = referenceTypes MigratedDues
  posted <- getPostedEntries fee.id.getId
  pure
    ( not (anyPosted posted [dailyFee, migratedFee]),
      not (anyPosted posted [dailyGst, migratedGst]),
      not (anyPosted posted [dailyPenalty, migratedPenalty])
    )

-- | True when a fee has a component with a non-zero amount that is not in the ledger: nothing posted
--   yet, or only part of it.
walletDebitIncomplete ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r, BeamFlow m r) =>
  DDF.DriverFee ->
  m Bool
walletDebitIncomplete fee = do
  (feeMissing, gstMissing, penaltyMissing) <- missingDebitComponents fee
  let (feeAmount, gstAmount, penaltyAmount) = feeDebitAmounts fee
  pure $ (feeAmount > 0 && feeMissing) || (gstAmount > 0 && gstMissing) || (penaltyAmount > 0 && penaltyMissing)

-- | Debit one fee (platform fee → SellerRevenue, GST → GovtIndirect, any folded penalty →
--   OwnerExpense), then mark it CLEARED (collected_by = WALLET). No invoice is written: the
--   ledger entries (reference = driver fee id) are the record of the payment. Migrated dues also
--   have their old invoices deactivated, which kills any open payment link.
debitDriverFeeFromWallet ::
  ( MonadFlow m,
    EsqDBFlow m r,
    CacheFlow m r,
    BeamFlow m r,
    HasActorInfo m r,
    Redis.HedisFlow m r,
    Redis.HedisLTSFlowEnv r
  ) =>
  DTC.TransporterConfig ->
  WalletDebitKind ->
  DDF.DriverFee ->
  m ()
debitDriverFeeFromWallet transporterConfig kind fee = do
  (counterpartyType, ownerId) <- resolveWalletOwner (cast fee.driverId)
  let (feeRef, gstRef, penaltyRef) = referenceTypes kind
      (feeAmount, gstAmount, penaltyAmount) = feeDebitAmounts fee
      ctx = buildDriverChargeCtx counterpartyType ownerId fee.merchantId.getId fee.merchantOperatingCityId.getId fee.currency fee.id.getId transporterConfig.driverWalletConfig
  Redis.withWaitAndLockRedis (makeWalletRunningBalanceLockKey ownerId) 10 1000 $ do
    -- Each component is posted only if it is not already in the ledger (under either kind), so a retry after
    -- a crash between the three transfers posts exactly what is missing and never debits twice.
    (feeMissing, gstMissing, penaltyMissing) <- missingDebitComponents fee
    let postFee = feeMissing && feeAmount > 0
        postGst = gstMissing && gstAmount > 0
        postPenalty = penaltyMissing && penaltyAmount > 0
    when (postFee || postGst || postPenalty) $ do
      result <- runFinance ctx $ do
        when postFee $ void $ transfer OwnerLiability SellerRevenue feeAmount feeRef Nothing
        when postGst $ void $ transfer OwnerLiability GovtIndirect gstAmount gstRef Nothing
        when postPenalty $ void $ transfer OwnerLiability OwnerExpense penaltyAmount penaltyRef Nothing
      case result of
        Left err -> throwError $ InternalError ("Wallet debit failed for driver fee " <> fee.id.getId <> ": " <> show err)
        Right _ -> logInfo $ "Debited driver fee " <> fee.id.getId <> " (" <> show kind <> ") from wallet of " <> ownerId
    -- Only after the ledger post: a crash in between retries into the missing-components check. Still under
    -- the wallet lock, so two jobs on the same fee can't interleave here.
    now <- getCurrentTime
    QDF.updateCollectedPaymentStatus DDF.CLEARED (Just walletCollectedBy) now Nothing fee.id
    -- A migrated due's old invoices are deactivated, which kills any open payment link for it.
    when (kind == MigratedDues) $
      QINV.updateInvoiceStatusByDriverFeeIdsAndMbPaymentMode INV.INACTIVE [fee.id] Nothing

debitBehaviourPenaltyFromWallet ::
  ( MonadFlow m,
    EsqDBFlow m r,
    CacheFlow m r,
    BeamFlow m r,
    HasActorInfo m r,
    Redis.HedisFlow m r,
    Redis.HedisLTSFlowEnv r
  ) =>
  DTC.TransporterConfig ->
  Text ->
  Text ->
  Id DP.Person ->
  HighPrecMoney ->
  Text ->
  m ()
debitBehaviourPenaltyFromWallet transporterConfig merchantIdText merchantOpCityIdText driverId penaltyMoney penaltyChargeRef = do
  (counterpartyType, ownerId) <- resolveWalletOwner driverId
  let ctx = buildDriverChargeCtx counterpartyType ownerId merchantIdText merchantOpCityIdText transporterConfig.currency penaltyChargeRef transporterConfig.driverWalletConfig
  Redis.withWaitAndLockRedis (makeWalletRunningBalanceLockKey ownerId) 10 1000 $ do
    existing <- getEntriesByReference walletReferenceBehaviourPenalty penaltyChargeRef
    when (null existing) $ do
      result <- runFinance ctx $ void $ transfer OwnerLiability OwnerExpense penaltyMoney walletReferenceBehaviourPenalty Nothing
      case result of
        Left err -> throwError $ InternalError ("Wallet penalty debit failed for driver " <> driverId.getId <> " charge " <> penaltyChargeRef <> ": " <> show err)
        Right _ -> logInfo $ "Debited penalty " <> penaltyChargeRef <> " of " <> show penaltyMoney <> " from wallet of " <> ownerId

-- | Called by 'processPayment' once a payment order succeeds, with the fees as they were before
--   it marked them CLEARED:
--
--   * a WALLET_TOPUP fee (Autopay recharge) is credited to the driver's own wallet -- the money
--     came from the driver's mandate, so it never lands in a fleet wallet;
--   * a fee already collected from the wallet (a migrated due, paid late through an old link or
--     a late webhook) is credited back to the wallet that was debited, instead of being collected
--     twice.
--
--   Each credit is posted at most once (reference checked under the wallet lock).
creditWalletForPaidDriverFees ::
  ( MonadFlow m,
    EsqDBFlow m r,
    CacheFlow m r,
    BeamFlow m r,
    HasActorInfo m r,
    Redis.HedisFlow m r,
    Redis.HedisLTSFlowEnv r
  ) =>
  DTC.TransporterConfig ->
  DP.Person ->
  Text ->
  [DDF.DriverFee] ->
  m ()
creditWalletForPaidDriverFees transporterConfig driver paymentOrderId feesBeforePayment = do
  -- processPayment marks the fee CLEARED before calling this, so a retry after a failed credit sees it
  -- CLEARED already. Idempotency comes from the reference check in creditWallet, not from the status --
  -- which also means an INACTIVE recharge counts: a charge that timed out client-side was marked INACTIVE,
  -- but if the gateway did charge the mandate the paid order is the proof, and it must be credited.
  let recharges = filter ((== DDF.WALLET_TOPUP) . (.feeType)) feesBeforePayment
      latePaidWalletFees = filter (\df -> df.status == DDF.CLEARED && df.collectedBy == Just walletCollectedBy) feesBeforePayment
  forM_ recharges $ \fee -> do
    credited <- creditWallet (DRIVER, fee.driverId.getId) walletReferenceAutopayRecharge fee.id.getId fee
    when credited $ do
      let title = "Wallet recharged"
          message = "Rs." <> show (feeAmount fee) <> " has been added to your wallet by Autopay."
      fork "wallet autopay recharge notification" $
        sendNotificationToDriver driver.merchantOperatingCityId FCM.SHOW Nothing FCM.DRIVER_NOTIFY title message driver driver.deviceToken
  forM_ latePaidWalletFees $ \fee -> do
    owner <- resolveWalletOwner (cast fee.driverId)
    void $ creditWallet owner walletReferenceLatePaymentCredit (paymentOrderId <> ":" <> fee.id.getId) fee
  where
    feeAmount fee = fee.platformFee.fee + fee.platformFee.cgst + fee.platformFee.sgst + fee.govtCharges + fromMaybe 0 fee.cancellationPenaltyAmount
    creditWallet (counterpartyType, ownerId) referenceType referenceId fee =
      Redis.withWaitAndLockRedis (makeWalletRunningBalanceLockKey ownerId) 10 1000 $ do
        existing <- getEntriesByReference referenceType referenceId
        if not (null existing)
          then pure False
          else do
            let ctx = buildDriverChargeCtx counterpartyType ownerId fee.merchantId.getId fee.merchantOperatingCityId.getId fee.currency referenceId transporterConfig.driverWalletConfig
            result <- runFinance ctx $ void $ transfer PlatformAsset OwnerLiability (feeAmount fee) referenceType Nothing
            case result of
              Left err -> throwError $ InternalError ("Wallet credit failed for driver fee " <> fee.id.getId <> ": " <> show err)
              Right _ -> do
                logInfo $ "Credited " <> referenceType <> " for driver fee " <> fee.id.getId <> " to wallet of " <> ownerId
                pure True
