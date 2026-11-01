{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- | Claiming one bulk payout beneficiary: re-check under the wallet lock, then hold and create the
--   order, or record an exclusion. The batch and its submission live in "Lib.Payment.Payout.Bulk".
module SharedLogic.Payout.Bulk.Claim
  ( claimBeneficiary,
    beneficiaryBankOf,
  )
where

import Control.Applicative ((<|>))
import Data.IORef (newIORef, readIORef, writeIORef)
import qualified Data.Text as T
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

-- | Claim one batched beneficiary: create their request and order, or only an EXCLUDED request when
--   bank details are missing. Checks re-run under the wallet lock, as the balance may have moved since.
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
  -- | 'Bulk.Claimed' carries the checked bank details forward so nothing reads them twice.
  --   'Bulk.Excluded' holds nothing and is never sent. 'Bulk.Dropped' is not paid this run.
  m Bulk.ClaimResult
claimBeneficiary config payoutType transporterConfig payoutServiceName merchantId merchantOpCityId batch candidate = do
  let personId = candidate.beneficiary.id
      walletLockKey = makeWalletRunningBalanceLockKey personId.getId
  -- Decrypted before the locks: it calls the encryption service, and ride end waits on the inner lock.
  customerPhone <- mapM decrypt candidate.beneficiary.mobileNumber
  -- Stays empty only when the inner lock could not be taken.
  resultRef <- liftIO $ newIORef Nothing
  let decide = liftIO . writeIORef resultRef . Just
  result <- try $
    -- Outer: the lock every wallet payout takes, so two payouts never hold the same money (30 s, as the
    -- inner wait alone can take 10 s). Inner: the balance lock ride end and settlement take.
    -- Always in this order; never take a payout lock inside the inner one.
    PayoutRequest.runPayoutUnderLock walletLockKey 30 (pure (Just ())) $ \() ->
      Redis.withWaitAndLockRedisWithTimeout walletLockKey 10 10 50000 $ do
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
                -- Excluded at the eligibility pass stays excluded this run: the batch was sized without
                -- them, so paying now could exceed the partner's item cap. The next run pays them.
                mbExclusionReason = fresh.exclusionReason <|> candidate.exclusionReason
            case (mbExclusionReason, standing.mbBankAccount) of
              (Just reason, _) -> do
                reqId <- Bulk.recordExclusion ctx.merchantId.getId ctx.mocId.getId DPayment.DRIVER_WALLET_TRANSACTION ctx.driverId.getId batch standing.payoutableBalance payoutType reason
                notifyExcludedBeneficiary candidate.beneficiary standing.payoutableBalance reason
                decide (Bulk.Excluded reqId reason)
              (Nothing, Just bankAccount) ->
                -- Built before anything is held, so details HDFC would reject drop the claim instead.
                case beneficiaryBankOf bankAccount of
                  Left reason -> do
                    logError $ "BulkPayoutClaim: dropping " <> personId.getId <> " at claim time -- " <> reason
                    decide (Bulk.Dropped reason)
                  Right bank -> do
                    mbOrder <-
                      initiateWalletPayoutWith
                        ctx
                        standing.payoutableBalance
                        payoutType
                        Nothing
                        standing.cutoff
                        standing.redeemableEntryIds
                        standing.merchantTransferAmount
                        (Just batch.id)
                        -- The batch's own service and the bank account just checked above.
                        (Just PayoutPrefetch {route = (Payout.BulkFlow, payoutServiceName, Just bankAccount), customerPhone})
                        -- No per-order status check: the BulkPayoutStatusCheck job resolves the whole batch.
                        (\_ -> pure ())
                    decide $ case mbOrder of
                      Just order -> Bulk.Claimed order bank
                      -- Nothing to pay after the fee, or the hold or order failed (money already given back).
                      Nothing -> Bulk.Dropped "Payout not sent: nothing to pay after the fee, or the hold or order failed"
              -- Unreachable: no exclusionReason means a usable account is present. Logged, nothing claimed.
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

-- | The trimmed bank details sent to the partner. 'Left' should not happen after 'usableBankDetails';
--   if it does, the caller drops the beneficiary before anything is held.
beneficiaryBankOf :: Domain.Types.DriverBankAccount.DriverBankAccount -> Either Text Bulk.BeneficiaryBank
beneficiaryBankOf bankAccount = do
  accountNumber <- required "Bank account number missing" (Just bankAccount.accountId)
  ifscCode <- required "IFSC code missing" bankAccount.ifscCode
  holderName <- required "Account holder name missing" bankAccount.nameAtBank
  pure Bulk.BeneficiaryBank {accountNumber, ifscCode, holderName}
  where
    required reason mbValue = case T.strip <$> mbValue of
      Just value | not (T.null value) -> Right value
      _ -> Left reason

-- | Tell a driver their payout waits on fixing their bank details. At most once a day per reason,
--   since every sweep run re-excludes the same people. Drivers only (fleet owners have no push
--   channel yet); forked so a slow push never holds the wallet lock.
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
