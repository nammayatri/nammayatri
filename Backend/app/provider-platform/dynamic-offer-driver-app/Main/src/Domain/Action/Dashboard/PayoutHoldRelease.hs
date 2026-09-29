{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- | Admin actions for payout money stuck on hold: give it back when it is known not to have gone
--   out, or check a stopped bulk batch once more.
module Domain.Action.Dashboard.PayoutHoldRelease
  ( releaseHolds,
    recheckBatch,
  )
where

import qualified API.Types.ProviderPlatform.Management.Payout as ApiPayout
import Data.List (nub)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Environment
import qualified Kernel.External.Payout.Interface as Payout
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.APISuccess (APISuccess (Success))
import qualified Kernel.Types.Beckn.Context
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.Finance.Ledger.PayoutSettlement (findActivePayoutHold)
import qualified Lib.Payment.Domain.Types.PayoutBatch as DPayoutBatch
import qualified Lib.Payment.Domain.Types.PayoutOrder as DPayoutOrder
import qualified Lib.Payment.Domain.Types.PayoutRequest as PR
import Lib.Payment.Payout.Bulk.Schedule (batchLockKey, ensureCityJob)
import Lib.Payment.Payout.Bulk.Types (BulkFinalOutcome (BulkRejected), Handle)
import Lib.Payment.Payout.HoldRelease
import qualified Lib.Payment.Payout.Request as PayoutRequest
import qualified Lib.Payment.Storage.Queries.PayoutBatch as QPayoutBatch
import qualified Lib.Payment.Storage.Queries.PayoutOrder as QPayoutOrder
import qualified Lib.Payment.Storage.Queries.PayoutRequest as QPR
import qualified Lib.Payment.Storage.Queries.PayoutRequestExtra as QPayoutRequestExtra
import SharedLogic.Finance.Wallet (walletReferencePayout)
import SharedLogic.Finance.WalletPayout (reversalPendingPrefix)
import SharedLogic.Payout.Bulk.Driver (driverBulkHandle, rerunWalletLedgerStep, settleBulkItemWith)
import qualified Storage.CachedQueries.Merchant as QM
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC

-- | Most payout requests one release call takes: one HDFC file at most.
maxRequestsPerCall :: Int
maxRequestsPerCall = 500

-- | How long a release holds a batch's lock. Long enough for a full file; the items it touches
--   are ones the status check does not work on, so an expired lock does no harm.
releaseLockSeconds :: Int
releaseLockSeconds = 300

-- | Driver bulk handle, so a release shares the status check's batch lock.
bulkHandle :: Handle Environment.Flow
bulkHandle = driverBulkHandle

-- | One payout request as it is now, and what the rules say about it.
data Loaded = Loaded
  { request :: PR.PayoutRequest,
    mbOrder :: Maybe DPayoutOrder.PayoutOrder,
    check :: ReleaseCheck
  }

-- | Give held money back. Every request is checked first and if any is refused nothing moves; then
--   each batch is released under its lock from freshly read rows.
releaseHolds ::
  ShortId DM.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Maybe Text ->
  ApiPayout.PayoutReleaseHoldReq ->
  Environment.Flow ApiPayout.PayoutReleaseHoldResp
releaseHolds merchantShortId opCity mbRequestorId req = do
  merchantOpCity <- resolveCity merchantShortId opCity
  let ids = nub req.payoutRequestIds
      reason = T.strip req.reason
  when (T.null reason) $ throwError (InvalidRequest "reason is required: say why this money is being given back")
  when (null ids) $ throwError (InvalidRequest "payoutRequestIds is empty")
  when (length ids > maxRequestsPerCall) $
    throwError (InvalidRequest ("At most " <> show maxRequestsPerCall <> " payout requests per call"))
  requests <- filter (inCity merchantOpCity) <$> QPayoutRequestExtra.findByIds ids
  let missing = filter (`notElem` map (.id.getId) requests) ids
  unless (null missing) $
    throwError (InvalidRequest ("Payout requests not found in this city: " <> T.intercalate ", " missing))
  loaded <- loadRequests requests
  let refused = [(l.request.id.getId, why) | l <- loaded, Refuse why <- [l.check]]
  unless (null refused) $
    throwError (InvalidRequest ("Nothing was released. Refused: " <> T.intercalate "; " [rid <> ": " <> why | (rid, why) <- refused]))
  logInfo $ "Admin hold release by " <> fromMaybe "unknown" mbRequestorId <> " for " <> show (length ids) <> " payout request(s): " <> reason
  let byBatch = Map.fromListWith (flip (<>)) [(getId <$> r.batchId, [r]) | r <- requests]
  results <- concat <$> mapM (uncurry (releaseGroup reason)) (Map.toList byBatch)
  let resultsById = Map.fromList [(item.payoutRequestId, item) | item <- results]
  pure ApiPayout.PayoutReleaseHoldResp {results = mapMaybe (`Map.lookup` resultsById) ids}

-- | Release one batch's requests, or the batchless Juspay/Stripe ones. Uses the status check's lock
--   so the two cannot settle the same rows at once.
releaseGroup :: Text -> Maybe Text -> [PR.PayoutRequest] -> Environment.Flow [ApiPayout.PayoutReleaseHoldItem]
releaseGroup reason mbBatchId requests = case mbBatchId of
  Nothing -> loadRequests requests >>= mapM (releaseOne reason)
  Just batchId -> do
    locked <- Redis.whenWithLockRedisAndReturnValue (batchLockKey bulkHandle (Id batchId)) releaseLockSeconds $ do
      fresh <- QPayoutRequestExtra.findByIds (map (.id.getId) requests) >>= loadRequests
      items <- mapM (releaseOne reason) fresh
      closeBatchIfNothingOpen reason (Id batchId)
      pure items
    case locked of
      Right items -> pure items
      Left () -> pure [failed r.id.getId "This batch is being checked right now; try again in a minute" | r <- requests]

-- | Give back one request's money, as the rules decided for the rows read inside the lock.
releaseOne :: Text -> Loaded -> Environment.Flow ApiPayout.PayoutReleaseHoldItem
releaseOne reason l = case l.check of
  NothingHeld -> pure (item ApiPayout.NOTHING_HELD Nothing)
  Refuse why -> pure (failed requestId why)
  Release how -> do
    res <- try (release how)
    case res of
      Right () -> pure (item ApiPayout.RELEASED Nothing)
      Left (err :: SomeException) -> do
        logError $ "Admin hold release failed for payout request " <> requestId <> ": " <> show err
        pure (failed requestId (T.take 200 (show err)))
  where
    pr = l.request
    requestId = pr.id.getId
    item result message = ApiPayout.PayoutReleaseHoldItem {payoutRequestId = requestId, result, message}
    releasedReason = releasedByAdminPrefix <> reason

    release = \case
      -- Settled like a file the bank refused, but with no push since an admin is giving the money back.
      FailInFlightBulkItem -> do
        order <- fromMaybeM (InternalError ("No payout order for payout request " <> requestId)) l.mbOrder
        settleBulkItemWith False pr.merchantOperatingCityId order (Just pr) (BulkRejected (Just "RELEASED_BY_ADMIN") (Just releasedReason) Nothing Nothing)
      -- Only the ledger step: the hold goes back to the wallet.
      ReverseHoldOnly -> do
        let merchantId = maybe pr.merchantId (.merchantId) l.mbOrder
            personId = maybe pr.beneficiaryId (.customerId) l.mbOrder
            amount = maybe (fromMaybe 0 pr.amount) (.amount.amount) l.mbOrder
        rerunWalletLedgerStep pr.merchantOperatingCityId merchantId personId pr amount Payout.FAILURE (const (pure ()))
        unless (pr.status == PR.AUTO_PAY_FAILED) $
          PayoutRequest.updateStatusWithHistoryById PR.AUTO_PAY_FAILED (Just releasedReason) pr
        QPR.updateStatusWithReasonById PR.AUTO_PAY_FAILED (Just releasedReason) pr.id

-- | Close a stopped batch once a release leaves nothing open: SUBMIT_FAILED if HDFC never took it,
--   else COMPLETED. A REVERSAL_PENDING request still holds money, so it counts as open.
closeBatchIfNothingOpen :: Text -> Id DPayoutBatch.PayoutBatch -> Environment.Flow ()
closeBatchIfNothingOpen reason batchId = do
  mbBatch <- QPayoutBatch.findByPrimaryKey batchId
  whenJust mbBatch $ \batch -> do
    orders <- QPayoutOrder.findAllByBatchId (Just batchId)
    requests <- QPayoutRequestExtra.findByIds (mapMaybe requestIdOf orders)
    let requestsById = Map.fromList [(r.id.getId, r) | r <- requests]
        settledStatusOf o = do
          r <- (`Map.lookup` requestsById) =<< requestIdOf o
          guard (not (isReversalPending r))
          pure r.status
    whenJust (closedStatusAfterRelease batch (map (.status) orders) (map settledStatusOf orders)) $ \closed -> do
      now <- getCurrentTime
      logInfo $ "Payout batch " <> batchId.getId <> ": nothing left open after an admin release; " <> show closed
      QPayoutBatch.updateAfterStatusCall closed (Just now) (Just ("Held money given back by an admin: " <> reason)) batch.failureCode batch.statusCheckRound 0 Nothing batch.statusNoDataReplies batchId

-- | Put a batch that stopped in MANUAL_REVIEW_REQUIRED back for one more check of its plan. One
--   write under the batch's lock makes it due now; the city's status-check job makes the call.
recheckBatch ::
  ShortId DM.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Text ->
  Maybe Text ->
  ApiPayout.PayoutBatchRecheckReq ->
  Environment.Flow APISuccess
recheckBatch merchantShortId opCity batchIdText mbRequestorId req = do
  merchantOpCity <- resolveCity merchantShortId opCity
  let reason = T.strip req.reason
      batchId = Id batchIdText
  when (T.null reason) $ throwError (InvalidRequest "reason is required: say why this batch is being checked again")
  batch <- QPayoutBatch.findByPrimaryKey batchId >>= fromMaybeM (InvalidRequest ("PayoutBatch not found: " <> batchIdText))
  unless (batch.merchantId == merchantOpCity.merchantId.getId && batch.merchantOperatingCityId == merchantOpCity.id.getId) $
    throwError (InvalidRequest ("PayoutBatch not found: " <> batchIdText))
  locked <- Redis.whenWithLockRedisAndReturnValue (batchLockKey bulkHandle batchId) 120 $ do
    fresh <- QPayoutBatch.findByPrimaryKey batchId >>= fromMaybeM (InvalidRequest ("PayoutBatch not found: " <> batchIdText))
    either (throwError . InvalidRequest) pure (checkRecheck fresh)
    now <- getCurrentTime
    let note = "Re-check by " <> fromMaybe "admin" mbRequestorId <> ": " <> reason <> maybe "" (". Before: " <>) fresh.failureReason
    logInfo $ "Payout batch " <> batchIdText <> ": " <> note
    -- Calls reset to 0 for a full check; the round is kept so the plan then decides as usual.
    QPayoutBatch.updateAfterStatusCall (recheckStatus fresh) Nothing (Just note) fresh.failureCode fresh.statusCheckRound 0 (Just now) fresh.statusNoDataReplies batchId
  case locked of
    Left () -> throwError (InvalidRequest "This batch is being checked right now; try again in a minute")
    Right () -> pure ()
  -- Usually a no-op, as the city's job reschedules itself; it only creates one when none is pending.
  ensureCityJob bulkHandle batch.merchantId batch.merchantOperatingCityId
  pure Success

-- | Load each request's order and hold and run the release rules. A batch's orders are read once.
loadRequests :: [PR.PayoutRequest] -> Environment.Flow [Loaded]
loadRequests requests = do
  let batchIds = nub (mapMaybe (.batchId) requests)
  batches <- forM batchIds $ \batchId -> do
    batch <- QPayoutBatch.findByPrimaryKey batchId >>= fromMaybeM (InvalidRequest ("PayoutBatch not found: " <> batchId.getId))
    orders <- QPayoutOrder.findAllByBatchId (Just batchId)
    pure (batchId, (batch, Map.fromList [(rid, o) | o <- orders, Just rid <- [requestIdOf o]]))
  let batchesById = Map.fromList batches
  forM requests $ \pr -> do
    (flow, mbOrder) <- case pr.batchId >>= (`Map.lookup` batchesById) of
      Just (batch, ordersByRequest) -> pure (BulkPayout batch, Map.lookup pr.id.getId ordersByRequest)
      -- Juspay/Stripe: the request keeps its order's id.
      Nothing -> (GatewayPayout,) <$> maybe (pure Nothing) (QPayoutOrder.findById . Id) pr.payoutTransactionId
    hold <- findActivePayoutHold walletReferencePayout pr.id.getId
    let state =
          HoldState
            { flow,
              orderStatus = (.status) <$> mbOrder,
              requestStatus = pr.status,
              reversalPending = isReversalPending pr,
              holdActive = isJust hold
            }
    pure Loaded {request = pr, mbOrder, check = checkRelease state}

isReversalPending :: PR.PayoutRequest -> Bool
isReversalPending pr = maybe False (reversalPendingPrefix `T.isPrefixOf`) pr.failureReason

requestIdOf :: DPayoutOrder.PayoutOrder -> Maybe Text
requestIdOf order = listToMaybe =<< order.entityIds

inCity :: DMOC.MerchantOperatingCity -> PR.PayoutRequest -> Bool
inCity merchantOpCity pr = pr.merchantId == merchantOpCity.merchantId.getId && pr.merchantOperatingCityId == merchantOpCity.id.getId

failed :: Text -> Text -> ApiPayout.PayoutReleaseHoldItem
failed requestId why = ApiPayout.PayoutReleaseHoldItem {payoutRequestId = requestId, result = ApiPayout.RELEASE_FAILED, message = Just why}

resolveCity :: ShortId DM.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.Flow DMOC.MerchantOperatingCity
resolveCity merchantShortId opCity = do
  merchant <- QM.findByShortId merchantShortId >>= fromMaybeM (MerchantDoesNotExist merchantShortId.getShortId)
  CQMOC.findByMerchantIdAndCity merchant.id opCity >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchant-Id-" <> merchant.id.getId <> "-city-" <> show opCity)
