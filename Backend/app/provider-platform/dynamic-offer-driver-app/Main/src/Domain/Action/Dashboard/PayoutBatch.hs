{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.Dashboard.PayoutBatch
  ( listPayoutBatches,
    listPayoutBatchOrders,
  )
where

import qualified API.Types.ProviderPlatform.Management.Payout as ApiPayout
import qualified "dashboard-helper-api" Dashboard.Common as DC
import Data.List (nub)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.PayoutRun as DDPR
import qualified Domain.Types.Person as DP
import qualified Environment
import Kernel.External.Encryption (decrypt)
import Kernel.Prelude
import qualified Kernel.Types.Beckn.Context
import Kernel.Types.Error
import qualified Kernel.Types.Id as Id
import Kernel.Utils.Common
import qualified Lib.Payment.Domain.Types.PayoutBatch as DPayoutBatch
import qualified Lib.Payment.Domain.Types.PayoutOrder as DPayoutOrder
import qualified Lib.Payment.Storage.Queries.PayoutBatch as QPayoutBatch
import qualified Lib.Payment.Storage.Queries.PayoutBatchExtra as QPayoutBatchExtra
import qualified Lib.Payment.Storage.Queries.PayoutOrderExtra as QPayoutOrderExtra
import SharedLogic.Allocator.Jobs.Payout.ScheduledBatchPayout (beneficiaryTypeFromRole)
import qualified Storage.CachedQueries.Merchant as QM
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.Queries.PayoutRun as QPayoutRun
import qualified Storage.Queries.Person as QPerson

-- | Paginated payout_batch list. Each row's runSummary (incl. excludedCount = skipped) comes
--   from the parent payout_run, since exclusions are recorded there, not per batch.
listPayoutBatches ::
  Id.ShortId DM.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Maybe Int ->
  Maybe Int ->
  Maybe UTCTime ->
  Maybe UTCTime ->
  Maybe DPayoutBatch.PayoutBatchStatus ->
  Maybe DPayoutBatch.PayoutBatchOrigin ->
  Maybe Text ->
  Environment.Flow ApiPayout.PayoutBatchListRes
listPayoutBatches merchantShortId opCity mbLimit mbOffset mbFrom mbTo mbStatus mbOrigin mbRail = do
  merchant <- QM.findByShortId merchantShortId >>= fromMaybeM (MerchantDoesNotExist merchantShortId.getShortId)
  merchantOpCity <- CQMOC.findByMerchantIdAndCity merchant.id opCity >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchant-Id-" <> merchant.id.getId <> "-city-" <> show opCity)
  let limit = min 20 (max 0 (fromMaybe 10 mbLimit))
      offset = max 0 (fromMaybe 0 mbOffset)
  batches <-
    QPayoutBatchExtra.findAllPayoutBatchesWithFilters
      merchant.id.getId
      merchantOpCity.id.getId
      mbFrom
      mbTo
      mbStatus
      mbOrigin
      mbRail
      (Just limit)
      (Just offset)
  -- One query for every unique runId on this page, not one query per batch.
  let runIds = nub $ mapMaybe (.runId) batches
  runRows <- QPayoutRun.findAllByIds (map Id.Id runIds)
  let runsById = Map.fromList [(r.id.getId, r) | r <- runRows]
      batchItems = map (toBatchListItem runsById) batches
      totalItems = length batchItems
  pure ApiPayout.PayoutBatchListRes {batches = batchItems, summary = DC.Summary {totalCount = 10000, count = length batchItems}, totalItems}

toBatchListItem :: Map Text DDPR.PayoutRun -> DPayoutBatch.PayoutBatch -> ApiPayout.PayoutBatchListItem
toBatchListItem runsById batch =
  ApiPayout.PayoutBatchListItem
    { id = batch.id.getId,
      runId = batch.runId,
      runSummary = toRunSummary <$> (batch.runId >>= (`Map.lookup` runsById)),
      origin = batch.origin,
      status = batch.status,
      payoutRail = batch.payoutRail,
      valueDate = batch.valueDate,
      clientRefNo = batch.clientRefNo,
      partnerBatchRef = batch.partnerBatchRef,
      itemCount = batch.itemCount,
      totalAmount = batch.totalAmount,
      processedCount = batch.processedCount,
      rejectedCount = batch.rejectedCount,
      pendingCount = batch.pendingCount,
      partnerResponseCode = batch.partnerResponseCode,
      failureReason = batch.failureReason,
      inquiryAttemptsToday = batch.inquiryAttemptsToday,
      inquiryQuotaDate = batch.inquiryQuotaDate,
      nextInquiryAt = batch.nextInquiryAt,
      submittedAt = batch.submittedAt,
      resolvedAt = batch.resolvedAt,
      createdAt = batch.createdAt,
      updatedAt = batch.updatedAt
    }

toRunSummary :: DDPR.PayoutRun -> ApiPayout.PayoutRunSummary
toRunSummary run =
  ApiPayout.PayoutRunSummary
    { evaluatedCount = run.evaluatedCount,
      excludedCount = run.excludedCount,
      includedCount = run.includedCount,
      paidCount = run.paidCount,
      failedCount = run.failedCount,
      pendingCount = run.pendingCount,
      totalAmount = run.totalAmount,
      paidAmount = run.paidAmount,
      failedAmount = run.failedAmount
    }

-- | Drill-down: every payout_order in one batch, with beneficiary identity attached for adhoc retry.
listPayoutBatchOrders ::
  Id.ShortId DM.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Text ->
  Maybe Int ->
  Maybe Int ->
  Environment.Flow ApiPayout.PayoutBatchOrdersRes
listPayoutBatchOrders merchantShortId _opCity batchId mbLimit mbOffset = do
  merchant <- QM.findByShortId merchantShortId >>= fromMaybeM (MerchantDoesNotExist merchantShortId.getShortId)
  batch <- QPayoutBatch.findByPrimaryKey (Id.Id batchId) >>= fromMaybeM (InvalidRequest $ "PayoutBatch not found: " <> batchId)
  -- Cross-merchant isolation: reject if this batch belongs to another merchant.
  unless (batch.merchantId == merchant.id.getId) $ throwError (InvalidRequest "PayoutBatch does not belong to this merchant")
  let offset = max 0 (fromMaybe 0 mbOffset)
      limit = min 20 (max 0 (fromMaybe 10 mbLimit))
  -- Paginated at the DB level; batch.itemCount (already fetched) is the total, no COUNT needed.
  orders <- QPayoutOrderExtra.findAllByBatchIdWithOptions batch.id.getId (Just limit) (Just offset)
  persons <- QPerson.findAllByPersonIds (nub $ map (.customerId) orders)
  let personsById = Map.fromList [(p.id.getId, p) | p <- persons]
  orderItems <- mapM (toOrderListItem personsById) orders
  pure ApiPayout.PayoutBatchOrdersRes {orders = orderItems, summary = DC.Summary {totalCount = batch.itemCount, count = length orderItems}}

toOrderListItem :: Map Text DP.Person -> DPayoutOrder.PayoutOrder -> Environment.Flow ApiPayout.PayoutOrderListItem
toOrderListItem personsById order = do
  let mbPerson = Map.lookup order.customerId personsById
  -- Don't let one bad decrypt fail the whole batch view.
  beneficiaryPhone <- case mbPerson >>= (.mobileNumber) of
    Nothing -> pure Nothing
    Just enc -> either (const Nothing) Just <$> try @_ @SomeException (decrypt enc)
  pure
    ApiPayout.PayoutOrderListItem
      { orderId = order.orderId,
        payoutRequestId = listToMaybe =<< order.entityIds,
        customerId = order.customerId,
        beneficiaryName = mbPerson <&> (.firstName),
        beneficiaryPhone,
        beneficiaryRole = maybe "UNKNOWN" (show . beneficiaryTypeFromRole . (.role)) mbPerson,
        status = show order.status,
        transferStatus = show <$> order.transferStatus,
        amount = order.amount.amount,
        failureCategory = order.failureCategory,
        settlementRef = order.settlementRef,
        settlementRefType = show <$> order.settlementRefType,
        createdAt = order.createdAt,
        updatedAt = order.updatedAt
      }
