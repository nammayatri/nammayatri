{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- NOTE (reviewer, remove before merge): NEW FILE (main has no bulk payout code). The "claim" step of the bulk lifecycle
--   (open batch -> CLAIM -> submit -> status check -> settle) for one person already placed in a batch. Opening the batch,
--   numbering the file and recording exclusions are in the payment lib (Lib.Payment.Payout.Bulk.Batch); only the
--   driver-app part is here.
--   Juspay/Stripe impact: bulk-only. The only caller is SharedLogic.Payout.Bulk.Driver.runBulkPayoutCycle (handed to the
--   lib's runBulkCycle), which is reached only for BulkFlow (HDFC) cities.

-- | The driver app's claim of one bulk payout beneficiary: re-check under the wallet lock, then hold
-- and create the order through the shared wallet payout, or record an exclusion. The batch, the
-- line items and the submission are in "Lib.Payment.Payout.Bulk".
module SharedLogic.Payout.Bulk.Claim
  ( claimBeneficiary,
    beneficiaryBankOf,
  )
where

import Control.Applicative ((<|>))
import Data.IORef (newIORef, readIORef, writeIORef)
import Domain.Action.UI.Ride.EndRide.Internal (makeWalletRunningBalanceLockKey)
import qualified Domain.Types.DriverBankAccount
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.MerchantServiceConfig as DEMSC
import qualified Domain.Types.Person as DP
import qualified Domain.Types.ScheduledPayoutConfig as DSPC
import qualified Domain.Types.TransporterConfig as DTConf
import Kernel.External.Encryption (decrypt)
import qualified Kernel.External.Notification.FCM.Types as FCM
import qualified Kernel.External.Payout.Interface as Payout
import Kernel.External.Types (ServiceFlow)
import Kernel.Prelude
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.Finance.Core.Types as Finance
import Lib.Finance.Storage.Beam.BeamFlow (BeamFlow)
import qualified Lib.Payment.Domain.Types.Common as DPayment
import qualified Lib.Payment.Domain.Types.PayoutBatch as DPayoutBatch
import qualified Lib.Payment.Domain.Types.PayoutRequest as PR
import qualified Lib.Payment.Payout.Bulk.Batch as Bulk
import qualified Lib.Payment.Payout.Bulk.Types as Bulk
import qualified Lib.Payment.Payout.Request as PayoutRequest
import qualified Lib.Payment.Storage.Beam.BeamFlow as PaymentBeamFlow
import SharedLogic.Finance.WalletPayout (PayoutContext (..), PayoutPrefetch (..), initiateWalletPayoutWith)
import SharedLogic.Payout.Bulk.Eligibility
import qualified Tools.Notifications as Notify

-- NOTE (reviewer, remove before merge): How the claim stays as safe as Juspay/Stripe, and uses their code:
--   * it takes BOTH per-person wallet locks. The key text is the same (makeWalletRunningBalanceLockKey), but two
--     helpers turn it into two different Redis keys:
--     - outer: PayoutRequest.runPayoutUnderLock -- the lock instant payout and the Juspay/Stripe sweep take
--       (WalletPayout.runWalletPayout; they hold it 10 s, the claim 30 s), so a claim and any other payout for this
--       person never hold the same money;
--     - inner: withWaitOnLockRedisWithExpiry -- the lock ride end and every payout settlement take, as on main.
--     Juspay/Stripe code and keys are as on main; only this claim takes both locks.
--   * it re-reads the payable amount with computePayoutableBalance (readPayoutSnapshot), the same function
--     WalletPayout.findWalletPayoutAmount uses for Juspay/Stripe instant and sweep payouts;
--   * the hold and the order are written by the shared WalletPayout.initiateWalletPayoutWith, hold first. So the same
--     money cannot be paid twice, for the same reason as Juspay/Stripe: wallet lock + hold.
--   Bulk-only arguments to the shared path: `Just batch.id`; a PayoutPrefetch (BulkFlow, the batch's service, the bank
--   account just checked), so nothing is read again under the lock; and `\_ -> pure ()`, i.e. no per-order status job.
--   Juspay/Stripe go through WalletPayout.initiateWalletPayout: no batch, no prefetch, main's per-order status job.

-- | Claim one already-batched beneficiary: create their payout_request and payout_order (or, if
--   they have no bank details, only an EXCLUDED request), both carrying the batch they belong to.
--   Every check is re-run inside the per-person balance lock, because the eligibility pass that
--   produced the candidate ran outside it and the balance (which another payout's hold may have
--   lowered) or the bank account may have moved since.
claimBeneficiary ::
  ( EncFlow m r,
    CacheFlow m r,
    Finance.HasActorInfo m r,
    EsqDBFlow m r,
    EsqDBReplicaFlow m r,
    BeamFlow m r,
    PaymentBeamFlow.BeamFlow m r,
    ServiceFlow m r,
    HasFlowEnv m r '["selfBaseUrl" ::: BaseUrl],
    Redis.HedisLTSFlowEnv r
  ) =>
  DSPC.ScheduledPayoutConfig ->
  PR.PayoutType -> -- SCHEDULED or ADHOC
  DTConf.TransporterConfig ->
  DEMSC.ServiceName -> -- the bulk payout service the batch is submitted through
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  DPayoutBatch.PayoutBatch ->
  BulkCandidate ->
  -- | 'Bulk.Claimed': claimed and ready to submit, with the bank details these checks passed on --
  --   carried forward so nothing reads them a second time and no later check can disagree.
  --   'Bulk.Excluded': excluded for want of usable bank details -- recorded as an EXCLUDED
  --   payout_request, never submitted, nothing held. 'Bulk.Dropped': not paid in this run, with the
  --   reason -- the re-check said no, nothing was left to pay, the wallet lock stayed busy, or an error.
  m Bulk.ClaimResult
claimBeneficiary config payoutType transporterConfig payoutServiceName merchantId merchantOpCityId batch candidate = do
  let personId = candidate.beneficiary.id
      walletLockKey = makeWalletRunningBalanceLockKey personId.getId
  -- Decrypted before the locks: it is a call to the encryption service, and ride-end waits on the
  -- inner lock.
  customerPhone <- mapM decrypt candidate.beneficiary.mobileNumber
  -- What the claim decided. Still empty afterwards only when the inner lock could not be taken.
  resultRef <- liftIO $ newIORef Nothing
  let decide = liftIO . writeIORef resultRef . Just
  result <- try $
    -- Outer: the payout lock instant payout and the Juspay/Stripe sweep take (WalletPayout.runWalletPayout),
    -- so a claim and any other payout for this person never hold the same money. It waits until the lock is
    -- free. Held for 30 s rather than their 10 s because the inner wait can itself take 10 s.
    -- Inner: the wallet-balance lock ride end and settlement take; gives up after 10 s.
    -- Always taken in this order, and nothing may take the inner lock and then a payout lock inside it.
    PayoutRequest.runPayoutUnderLock walletLockKey 30 (pure (Just ())) $ \() ->
      Redis.withWaitOnLockRedisWithExpiry walletLockKey 10 10 $ do
        standing <- readPayoutSnapshot transporterConfig candidate.beneficiary
        case classifyBulkCandidate config candidate.beneficiary standing of
          Left reason -> do
            logInfo $ "BulkPayoutClaim: dropping " <> personId.getId <> " at claim time -- " <> reason
            decide (Bulk.Dropped reason)
          Right fresh -> do
            let ctx =
                  PayoutContext
                    { driverId = personId,
                      merchantId = merchantId,
                      mocId = merchantOpCityId,
                      person = candidate.beneficiary,
                      payoutVpa = Nothing,
                      transporterConfig = transporterConfig
                    }
                -- Excluded on the eligibility pass: stays excluded in this run, with that reason, even if
                -- the bank details were filled in since. The batch was sized without them, so paying them
                -- now could take it past the partner's item cap; the next run pays them.
                mbExclusionReason = fresh.exclusionReason <|> candidate.exclusionReason
            case (mbExclusionReason, standing.mbBankAccount) of
              (Just reason, _) -> do
                reqId <- Bulk.recordExclusion ctx.merchantId.getId ctx.mocId.getId DPayment.DRIVER_WALLET_TRANSACTION ctx.driverId.getId batch standing.payoutableBalance payoutType reason
                notifyExcludedBeneficiary candidate.beneficiary standing.payoutableBalance reason
                decide (Bulk.Excluded reqId)
              (Nothing, Just bankAccount) -> do
                mbOrder <-
                  initiateWalletPayoutWith
                    ctx
                    standing.payoutableBalance
                    payoutType
                    Nothing
                    (Just standing.cutoff)
                    standing.redeemableEntryIds
                    standing.merchantTransferAmount
                    (Just batch.id)
                    -- The route is already known here: the batch's own service, and the bank account
                    -- the snapshot above just read and checked. HDFC CBX has no mode or SDK variant.
                    (Just PayoutPrefetch {route = (Payout.BulkFlow, payoutServiceName, Just bankAccount), customerPhone})
                    -- No per-order status check on the bulk rail: the batch is resolved as a whole by
                    -- the BulkPayoutStatusCheck job.
                    (\_ -> pure ())
                decide $ case mbOrder of
                  Just order -> Bulk.Claimed order (beneficiaryBankOf bankAccount)
                  -- Nothing to pay after the fee, or the hold or the local order failed; in the
                  -- latter case the shared path has already given the money back.
                  Nothing -> Bulk.Dropped "Payout not sent: nothing to pay after the fee, or the hold or order failed"
              -- Unreachable: 'classifyBulkCandidate' leaves exclusionReason empty only when the account
              -- is present and usable. Reported rather than guessed at, and nothing is claimed.
              (Nothing, Nothing) -> do
                logError $ "BulkPayoutClaim: " <> personId.getId <> " passed every check with no bank account on file; not claimed"
                decide (Bulk.Dropped "No bank account on file")
  case result of
    Left (e :: SomeException) -> do
      logError $ "BulkPayoutClaim error for " <> personId.getId <> ": " <> show e
      pure (Bulk.Dropped "Claim failed with an error; see the logs")
    Right () ->
      liftIO (readIORef resultRef) >>= \case
        Just claimResult -> pure claimResult
        Nothing -> do
          logWarning $ "BulkPayoutClaim: wallet lock for " <> personId.getId <> " still busy after 10 s; not claimed in this run"
          pure (Bulk.Dropped "Wallet busy; not claimed in this run")

-- NOTE (reviewer, remove before merge): exported because the instant payout in a bulk city
--   (Bulk.Driver.runInstantBulkPayout) builds its one file line with it too, after the same usableBankDetails check.

-- | What the partner is sent for this beneficiary. Non-blank by the time we are here:
--   'usableBankDetails' excluded anyone whose IFSC or name at bank was missing before an order was
--   ever created.
beneficiaryBankOf :: Domain.Types.DriverBankAccount.DriverBankAccount -> Bulk.BeneficiaryBank
beneficiaryBankOf bankAccount =
  Bulk.BeneficiaryBank
    { accountNumber = bankAccount.accountId,
      ifscCode = fromMaybe "" bankAccount.ifscCode,
      holderName = fromMaybe "" bankAccount.nameAtBank
    }

-- NOTE (reviewer, remove before merge): Bulk-only reminder push for an EXCLUDED person (exclusions exist only on bulk).
--   Main's Juspay/Stripe sweep has no such reminder, so nothing changes for them. The reason is one of "Bank account not
--   added", "Bank account number missing", "IFSC code missing" or "Account holder name missing".

-- | Tell a driver that money is waiting for them but cannot be paid until their bank details are
--   fixed, with the reason that excluded them.
--
--   At most once a day per person and reason: a scheduled sweep re-excludes the same people on every
--   run (hourly, if so configured), and a reminder each time would be noise. A new reason -- say the
--   account was added but its IFSC is missing -- is a new message. Drivers only, as with the payout
--   outcome notifications: fleet owners have no push channel yet. Forked, so a slow push never holds
--   the wallet lock this runs under.
notifyExcludedBeneficiary ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r, Redis.HedisLTSFlowEnv r) =>
  DP.Person ->
  HighPrecMoney ->
  Text -> -- why they were excluded
  m ()
notifyExcludedBeneficiary person amount reason =
  when (person.role `notElem` [DP.FLEET_OWNER, DP.FLEET_BUSINESS]) $ do
    firstToday <- Redis.setNxExpire ("BulkPayoutExclusionNotified:" <> person.id.getId <> ":" <> reason) (24 * 3600) True
    when firstToday $
      fork ("BulkPayoutExclusionNotify:" <> person.id.getId) $
        Notify.sendNotificationToDriver
          person.merchantOperatingCityId
          FCM.SHOW
          Nothing
          FCM.PAYOUT_VPA_REMINDER
          "Add your bank details to get paid"
          ("Rs." <> show amount <> " is ready for payout but could not be sent: " <> reason <> ". Please add or update your bank details.")
          person
          person.deviceToken
