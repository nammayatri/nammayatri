{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- | Writing the rows of a batch: the batch itself, then a payout_request (plus a payout_order unless
-- the beneficiary is excluded) for each member, then the line items to send.
module SharedLogic.Payout.Bulk.Claim
  ( openBulkBatch,
    claimBeneficiary,
    recordExclusion,
    assembleBulkItems,
    nextClientRefNo,
  )
where

import Data.IORef (newIORef, readIORef, writeIORef)
import qualified Data.Time as Time
import Domain.Action.UI.DriverWallet (PayoutPrefetch (..), initiateWalletPayout)
import Domain.Action.UI.Ride.EndRide.Internal (makeWalletRunningBalanceLockKey)
import qualified Domain.Types.DriverBankAccount
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.MerchantServiceConfig as DEMSC
import qualified Domain.Types.Person as DP
import qualified Domain.Types.ScheduledPayoutConfig as DSPC
import qualified Domain.Types.TransporterConfig as DTConf
import Kernel.Beam.Functions (runInMasterDbAndRedis)
import Kernel.External.Encryption (decrypt)
import qualified Kernel.External.Notification.FCM.Types as FCM
import qualified Kernel.External.Payout.Interface as Payout
import Kernel.External.Types (ServiceFlow)
import Kernel.Prelude
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.Finance.Core.Types as Finance
import Lib.Finance.Storage.Beam.BeamFlow (BeamFlow)
import qualified Lib.Payment.Domain.Types.Common as DPayment
import qualified Lib.Payment.Domain.Types.PayoutBatch as DPayoutBatch
import qualified Lib.Payment.Domain.Types.PayoutOrder as DPayoutOrder
import qualified Lib.Payment.Domain.Types.PayoutRequest as PR
import qualified Lib.Payment.Storage.Beam.BeamFlow as PaymentBeamFlow
import qualified Lib.Payment.Storage.Queries.PayoutBatch as QPayoutBatch
import qualified Lib.Payment.Storage.Queries.PayoutBatchExtra as QPayoutBatchExtra
import qualified Lib.Payment.Storage.Queries.PayoutRequest as QPR
import SharedLogic.Finance.WalletPayout (PayoutContext (..))
import SharedLogic.Payout.Bulk.Eligibility
import SharedLogic.Payout.Bulk.Types
import qualified SharedLogic.Payout.BulkStatusCheck as BSC
import qualified Tools.Notifications as Notify

-- | HDFC's file reference: six digits, unique per value date.
--
--   A counter per date in Redis, keyed on the city's own day -- the caller passes the same
--   executionDate that goes on the wire as @reqdexctndt@, because the bank's uniqueness is on
--   that pair. When its key is missing -- the first batch of the day, or a lost key -- it is
--   seeded from the largest reference already stored for that date, skipping 1000 ahead:
--   payout_batch is KV-backed, so a batch written seconds ago may not be readable from Postgres
--   yet. There are ~900,000 references a day and no need for them to be consecutive, so a gap
--   costs nothing.
nextClientRefNo ::
  (MonadFlow m, PaymentBeamFlow.BeamFlow m r, Redis.HedisLTSFlowEnv r) =>
  Time.Day ->
  m Text
nextClientRefNo executionDate = Redis.runInMasterCloudRedisCell $ do
  let key = "PayoutBatch:FileRefNo:" <> show executionDate
  mbCurrent :: Maybe Int <- Redis.get key
  when (isNothing mbCurrent) $ do
    -- Master, not the replica: the seed only runs when the key is missing, which is exactly when two
    -- batches can race, and a stale read there collides on (filerefno, reqdexctndt) at the bank.
    dbMax <- runInMasterDbAndRedis $ QPayoutBatchExtra.findMaxClientRefNo executionDate
    -- Only the first caller's seed wins; the rest just increment it. 25 hours: the reference
    -- only has to be unique within its own execution date, so the key needs to outlive that date
    -- by a little -- an hour of slack covers a batch opened at 23:59 local and any clock skew --
    -- and then go, rather than accumulate one key per date forever.
    void $ Redis.setNxExpire key (25 * 3600) (max 100000 (dbMax + 1000))
  n <- Redis.incr key
  -- HDFC's filerefno is six digits. Sending a seventh would be refused at best; stop here instead,
  -- before anything is claimed against the reference.
  when (n > 999999) $
    throwError $ InternalError ("HDFC file references exhausted for execution date " <> show executionDate)
  pure (show n)

-- | Open a payout_batch. Written before anything is sent, so a crash in between leaves something
--   to recover from: the row starts with a recovery time on it, which makes the status-check job
--   ask HDFC whether the batch ever arrived.
openBulkBatch ::
  ( MonadFlow m,
    PaymentBeamFlow.BeamFlow m r,
    Redis.HedisLTSFlowEnv r
  ) =>
  Payout.BulkStatusCheckPlan ->
  DEMSC.ServiceName ->
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  DPayoutBatch.PayoutBatchOrigin ->
  DPayoutBatch.PayoutBatchRail ->
  Time.Day ->
  Int -> -- items expected in this batch
  HighPrecMoney -> -- their total
  Int -> -- beneficiaries excluded for want of bank details
  m DPayoutBatch.PayoutBatch
openBulkBatch plan payoutServiceName merchantId merchantOpCityId origin rail executionDate itemCount totalAmount excludedCount = do
  now <- getCurrentTime
  batchIdRaw <- generateGUID
  clientRefNo <- nextClientRefNo executionDate
  let batch =
        DPayoutBatch.PayoutBatch
          { id = Id batchIdRaw,
            merchantId = merchantId.getId,
            merchantOperatingCityId = merchantOpCityId.getId,
            payoutServiceName = show payoutServiceName,
            origin = origin,
            status = DPayoutBatch.CREATED,
            payoutRail = rail,
            executionDate = executionDate,
            clientRefNo = clientRefNo,
            partnerBatchRef = Nothing,
            itemCount = itemCount,
            totalAmount = totalAmount,
            excludedCount = excludedCount,
            submittedAt = Nothing,
            statusCheckRound = 0,
            statusCheckCalls = 0,
            -- Set before the call, not after: if this process dies mid-submit, the job finds the
            -- batch here and asks HDFC whether it arrived, instead of leaving the money reserved
            -- behind a row nothing looks at. On the plan's own first check, so a batch that never
            -- got as far as a submit answer is chased on the same cadence as everything else.
            nextStatusCallAt = Just (BSC.firstCheckAt plan now),
            statusNoDataReplies = 0,
            failureReason = Nothing,
            failureCode = Nothing,
            resolvedAt = Nothing,
            createdAt = now,
            updatedAt = now
          }
  QPayoutBatch.create batch
  pure batch

-- | Claim one already-batched beneficiary: create their payout_request and payout_order (or, if
--   they have no bank details, only an EXCLUDED request), both carrying the batch they belong to.
--   Every check is re-run inside the per-person balance lock, because the eligibility pass that
--   produced the candidate ran outside it and the balance, the bank account or a competing payout
--   may all have moved since.
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
  Id DPayoutBatch.PayoutBatch ->
  BulkCandidate ->
  -- | 'Right': claimed and ready to submit, with the bank account these checks passed on -- carried
  --   forward so nothing reads it a second time and no later check can disagree. 'Left': excluded for
  --   want of usable bank details -- recorded as an EXCLUDED payout_request, never submitted, nothing
  --   reserved. 'Nothing': dropped by the re-check, nothing written.
  m (Maybe (Either (Id PR.PayoutRequest) (DPayoutOrder.PayoutOrder, Domain.Types.DriverBankAccount.DriverBankAccount)))
claimBeneficiary config payoutType transporterConfig payoutServiceName merchantId merchantOpCityId batchId candidate = do
  let personId = candidate.person.id
  -- Decrypted before the lock: it is a call to the encryption service, and ride-end waits on the
  -- same lock.
  customerPhone <- mapM decrypt candidate.person.mobileNumber
  resultRef <- liftIO $ newIORef Nothing
  result <- try $
    Redis.withWaitOnLockRedisWithExpiry (makeWalletRunningBalanceLockKey personId.getId) 10 10 $ do
      standing <- readPayoutSnapshot transporterConfig candidate.person
      case classifyBulkCandidate config candidate.person standing of
        Left reason -> logInfo $ "BulkPayoutClaim: dropping " <> personId.getId <> " at claim time -- " <> reason
        Right fresh -> do
          let ctx =
                PayoutContext
                  { driverId = personId,
                    merchantId = merchantId,
                    mocId = merchantOpCityId,
                    person = candidate.person,
                    payoutVpa = Nothing,
                    transporterConfig = transporterConfig
                  }
          case (fresh.exclusionReason, standing.mbBankAccount) of
            (Just reason, _) -> do
              reqId <- recordExclusion ctx batchId standing.payoutableBalance payoutType reason
              notifyExcludedBeneficiary candidate.person standing.payoutableBalance reason
              liftIO $ writeIORef resultRef (Just (Left reqId))
            (Nothing, Just bankAccount) -> do
              mbOrder <-
                initiateWalletPayout
                  ctx
                  standing.payoutableBalance
                  payoutType
                  Nothing
                  (Just standing.cutoff)
                  standing.redeemableEntryIds
                  standing.merchantTransferAmount
                  (Just batchId)
                  -- The route is already known here: the batch's own service, and the bank account
                  -- the snapshot above just read and checked. HDFC CBX has no mode or SDK variant.
                  (Just PayoutPrefetch {route = (Payout.BulkFlow, payoutServiceName, Just bankAccount), customerPhone})
                  -- No per-order status check on the bulk rail: the batch is resolved as a whole by
                  -- the BulkPayoutStatusCheck job.
                  (\_ -> pure ())
              liftIO $ writeIORef resultRef ((\order -> Right (order, bankAccount)) <$> mbOrder)
            -- Unreachable: 'classifyBulkCandidate' leaves exclusionReason empty only when the account
            -- is present and usable. Reported rather than guessed at, and nothing is claimed.
            (Nothing, Nothing) ->
              logError $ "BulkPayoutClaim: " <> personId.getId <> " passed every check with no bank account on file; not claimed"
  case result of
    Left (e :: SomeException) -> do
      logError $ "BulkPayoutClaim error for " <> personId.getId <> ": " <> show e
      pure Nothing
    Right () -> liftIO $ readIORef resultRef

-- | Record a beneficiary dropped for want of usable bank details. No payout_order is created --
--   nothing was submitted for them -- and no ledger entries are reserved, so the amount stays payable
--   on the next run once the details are fixed. The request carries the batch it was dropped from,
--   which is what makes the per-batch excluded list answerable without a payout_order to join
--   through, and the reason as given: it is what the excluded endpoint shows an operator.
recordExclusion ::
  (MonadFlow m, PaymentBeamFlow.BeamFlow m r) =>
  PayoutContext ->
  Id DPayoutBatch.PayoutBatch ->
  HighPrecMoney ->
  PR.PayoutType ->
  Text -> -- which detail is missing, shown on the excluded worklist
  m (Id PR.PayoutRequest)
recordExclusion ctx batchId amount payoutType reason = do
  now <- getCurrentTime
  reqId <- generateGUID
  QPR.create
    PR.PayoutRequest
      { id = Id reqId,
        batchId = Just batchId,
        beneficiaryId = ctx.driverId.getId,
        amount = Just amount,
        status = PR.EXCLUDED,
        failureReason = Just reason,
        payoutType = Just payoutType,
        entityName = Just DPayment.DRIVER_WALLET_TRANSACTION,
        entityId = ctx.driverId.getId,
        entityRefId = Nothing,
        ledgerEntryIds = Nothing,
        -- An exclusion has no usable bank account on file -- that is why it is excluded.
        bankName = Nothing,
        bankAccountLast4 = Nothing,
        merchantId = ctx.merchantId.getId,
        merchantOperatingCityId = ctx.mocId.getId,
        city = Nothing,
        coverageFrom = Nothing,
        coverageTo = Nothing,
        customerEmail = Nothing,
        customerName = Nothing,
        customerPhone = Nothing,
        customerVpa = Nothing,
        cashMarkedAt = Nothing,
        cashMarkedById = Nothing,
        cashMarkedByName = Nothing,
        expectedCreditTime = Nothing,
        orderType = Nothing,
        payoutFee = Nothing,
        payoutTransactionId = Nothing,
        remark = Nothing,
        retryCount = Nothing,
        scheduledAt = Nothing,
        createdAt = now,
        updatedAt = now
      }
  logInfo $ "BulkPayoutClaim: excluded " <> ctx.driverId.getId <> " from batch " <> batchId.getId <> " -- " <> reason
  pure (Id reqId)

-- | Tell a driver that money is waiting for them but cannot be paid until their bank details are
--   fixed, with the reason that excluded them.
--
--   At most once a day per person and reason: a scheduled sweep re-excludes the same people on every
--   run (hourly, if so configured), and a reminder each time would be noise. A new reason -- say the
--   account was added but its IFSC is wrong -- is a new message. Drivers only, as with the payout
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

-- | Turn claimed orders into the line items HDFC is sent.
--
--   Total by construction: every check that can reject a beneficiary already ran before their order
--   existed -- on the eligibility pass and again under the wallet lock at claim time -- and the bank
--   account that passed those checks is carried here rather than read again, so nothing can disagree
--   and no path creates an order and then drops it.
--
--   The one impossible case, an order with no short reference, throws: 'buildInitialPayoutOrder'
--   always assigns one, so a missing one is our own invariant broken rather than a problem with this
--   beneficiary, and reporting it as one would be a lie. Raised here rather than inside
--   'claimBeneficiary', whose @try@ would swallow it and leave the claim behind.
assembleBulkItems ::
  (MonadFlow m) =>
  [(DPayoutOrder.PayoutOrder, Domain.Types.DriverBankAccount.DriverBankAccount)] ->
  m [(DPayoutOrder.PayoutOrder, Payout.BulkPayoutItem)]
assembleBulkItems claimed = forM claimed $ \(order, bankAccount) -> do
  -- The reference we send is the order's SHORT id, not its id: HDFC cap custrefno at 20 characters
  -- and reject the whole item above it ("Length should not be more then 20"), and an order id is a
  -- 36-character UUID. The short id is what comes back on every inquiry row, so it is also how a
  -- response is matched to this order.
  shortId <-
    order.shortId
      & fromMaybeM (InternalError $ "Payout order " <> order.orderId <> " has no short reference; a bulk item cannot be built without one")
  pure
    ( order,
      Payout.BulkPayoutItem
        { itemRef = getShortId shortId,
          amount = order.amount.amount,
          currency = order.amount.currency,
          bankAccountNumber = bankAccount.accountId,
          -- Non-blank by the time we are here: 'usableBankDetails' excluded anyone whose IFSC or name
          -- at bank was missing before this order was ever created.
          bankIfscCode = fromMaybe "" bankAccount.ifscCode,
          beneficiaryName = fromMaybe "" bankAccount.nameAtBank,
          beneficiaryCode = Nothing,
          beneficiaryEmail = Nothing
        }
    )
