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
    listPayoutExcluded,
  )
where

import qualified API.Types.ProviderPlatform.Management.Payout as ApiPayout
import Data.List (nub)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Domain.Types.Merchant as DM
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
import qualified Lib.Payment.Domain.Types.PayoutRequest as DPayoutRequest
import qualified Lib.Payment.Storage.Queries.PayoutBatch as QPayoutBatch
import qualified Lib.Payment.Storage.Queries.PayoutBatchExtra as QPayoutBatchExtra
import qualified Lib.Payment.Storage.Queries.PayoutOrderExtra as QPayoutOrderExtra
import qualified Lib.Payment.Storage.Queries.PayoutRequestExtra as QPayoutRequestExtra
import qualified Storage.CachedQueries.Merchant as QM
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.Queries.Person as QPerson

-- | Hard ceiling on one page of any paginated payout list, and the page size when the caller
--   does not ask for one.
--
--   The ceiling is an abuse guard rather than a UX choice: without it a single request can ask for
--   an unbounded read, which is cheap amplification for anyone holding a token. It is deliberately
--   far above what a screen shows.
--
--   Capping is safe here only because 'hasMore' exists. The silent @min 20@ this replaces was a
--   bug precisely because a truncated page and a complete one looked identical -- there was no
--   honest total to compare against. With 'hasMore' the caller is told there is more and pages on.
maxPageSize :: Int
maxPageSize = 1000

-- | The page size the caller gets when they name one, bounded by 'maxPageSize'.
resolvePageSize :: Maybe Int -> Int
resolvePageSize mbLimit = min maxPageSize (max 0 (fromMaybe defaultPageSize mbLimit))

defaultPageSize :: Int
defaultPageSize = 20

-- | Paginated payout_batch list. Each row carries what was true when it was created (how many
--   items went to the partner, how many people were dropped for want of a bank account) plus how
--   far its status checks have got; per-order outcomes live on the payout_order rows themselves
--   (see listPayoutBatchOrders).
listPayoutBatches ::
  Id.ShortId DM.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Maybe Int ->
  Maybe Int ->
  Maybe UTCTime ->
  Maybe UTCTime ->
  Maybe DPayoutBatch.PayoutBatchStatus ->
  Maybe DPayoutBatch.PayoutBatchOrigin ->
  Maybe DPayoutBatch.PayoutBatchRail ->
  Environment.Flow ApiPayout.PayoutBatchListRes
listPayoutBatches merchantShortId opCity mbLimit mbOffset mbFrom mbTo mbStatus mbOrigin mbRail = do
  merchant <- QM.findByShortId merchantShortId >>= fromMaybeM (MerchantDoesNotExist merchantShortId.getShortId)
  merchantOpCity <- CQMOC.findByMerchantIdAndCity merchant.id opCity >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchant-Id-" <> merchant.id.getId <> "-city-" <> show opCity)
  let limit = resolvePageSize mbLimit
      offset = max 0 (fromMaybe 0 mbOffset)
  -- One row past the page. That extra row is the whole pagination signal: it answers "is there a
  -- next page" for the cost of a single row, where a real total would cost a scan of every batch
  -- matching the filters on every request.
  rows <-
    QPayoutBatchExtra.findAllPayoutBatchesWithFilters
      merchantOpCity.id.getId
      mbFrom
      mbTo
      mbStatus
      mbOrigin
      mbRail
      (Just (limit + 1))
      (Just offset)
  let hasMore = length rows > limit
      batchItems = map toBatchListItem (take limit rows)
  pure ApiPayout.PayoutBatchListRes {batches = batchItems, hasMore}

toBatchListItem :: DPayoutBatch.PayoutBatch -> ApiPayout.PayoutBatchListItem
toBatchListItem batch =
  ApiPayout.PayoutBatchListItem
    { id = batch.id.getId,
      origin = batch.origin,
      status = batch.status,
      payoutRail = batch.payoutRail,
      executionDate = batch.executionDate,
      clientRefNo = batch.clientRefNo,
      partnerBatchRef = batch.partnerBatchRef,
      itemCount = batch.itemCount,
      totalAmount = batch.totalAmount,
      excludedCount = batch.excludedCount,
      failureReason = batch.failureReason,
      -- The partner's own code, kept beside the prose: 'withCallError' lifts it out of HDFC's
      -- problem document into its own column precisely so "every batch that hit TH99401" is
      -- answerable, which reading failureReason cannot do.
      failureCode = batch.failureCode,
      statusCheckRound = batch.statusCheckRound,
      nextStatusCallAt = batch.nextStatusCallAt,
      statusNoDataReplies = batch.statusNoDataReplies,
      submittedAt = batch.submittedAt,
      resolvedAt = batch.resolvedAt,
      createdAt = batch.createdAt,
      updatedAt = batch.updatedAt
    }

-- | Coarse driver/fleet-owner label for display -- the only distinction the batch-orders view needs.
beneficiaryRoleLabel :: DP.Role -> Text
beneficiaryRoleLabel role
  | role `elem` [DP.FLEET_OWNER, DP.FLEET_BUSINESS] = "FLEET_OWNER"
  | otherwise = "DRIVER"

-- | Drill-down: every payout_order in one batch, with beneficiary identity attached for adhoc
--   retry, plus everyone the batch excluded.
listPayoutBatchOrders ::
  Id.ShortId DM.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Text ->
  Environment.Flow ApiPayout.PayoutBatchOrdersRes
listPayoutBatchOrders merchantShortId _opCity batchId = do
  merchant <- QM.findByShortId merchantShortId >>= fromMaybeM (MerchantDoesNotExist merchantShortId.getShortId)
  batch <- QPayoutBatch.findByPrimaryKey (Id.Id batchId) >>= fromMaybeM (InvalidRequest $ "PayoutBatch not found: " <> batchId)
  -- Cross-merchant isolation: reject if this batch belongs to another merchant.
  unless (batch.merchantId == merchant.id.getId) $ throwError (InvalidRequest "PayoutBatch does not belong to this merchant")
  -- Not paginated: a batch is bounded by the partner's maxItemsPerBatch, and the operator opening
  -- one wants all of it.
  orders <- QPayoutOrderExtra.findAllByBatchIdWithOptions batch.id.getId Nothing Nothing
  persons <- QPerson.findAllByPersonIds (nub $ map (.customerId) orders)
  let personsById = Map.fromList [(p.id.getId, p) | p <- persons]
  orderItems <- mapM (toOrderListItem personsById) orders
  -- Excluded beneficiaries never produced a payout_order -- nothing was submitted for them -- so
  -- they are read from payout_request, which is where the exclusion is recorded. Fetched only when
  -- the batch says there is something to fetch, so the ordinary batch costs no extra query.
  excludedItems <-
    if batch.excludedCount > 0
      then do
        excludedReqs <- QPayoutRequestExtra.findExcludedByBatchId batch.id.getId
        exPersons <- QPerson.findAllByPersonIds (nub $ map (.beneficiaryId) excludedReqs)
        let exById = Map.fromList [(p.id.getId, p) | p <- exPersons]
        pure $ map (toExcludedRequestItem exById) excludedReqs
      else pure []
  pure ApiPayout.PayoutBatchOrdersRes {orders = orderItems, excluded = excludedItems}

-- | An excluded beneficiary rendered from its payout_request. Mirrors toExcludedItem, which does
--   the same from a payout_order for the city-wide excluded list.
toExcludedRequestItem :: Map Text DP.Person -> DPayoutRequest.PayoutRequest -> ApiPayout.PayoutExcludedItem
toExcludedRequestItem personsById req =
  let mbPerson = Map.lookup req.beneficiaryId personsById
   in ApiPayout.PayoutExcludedItem
        { personId = req.beneficiaryId,
          personName = mbPerson <&> (.firstName),
          role = maybe "UNKNOWN" (beneficiaryRoleLabel . (.role)) mbPerson,
          amount = fromMaybe 0 req.amount,
          reason = fromMaybe "NO_PAYOUT_METHOD" req.failureReason,
          excludedAt = req.createdAt,
          payoutRequestId = req.id.getId,
          batchId = (.getId) <$> req.batchId
        }

-- | §6 Excluded list: beneficiaries dropped for want of a usable payout method (payout_request
--   status EXCLUDED, with the reason in failureReason), for a city over a date range. The admin
--   copies an id from here into the Create-Batch flow once the driver fixes their bank details.
listPayoutExcluded ::
  Id.ShortId DM.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Maybe Int ->
  Maybe Int ->
  Maybe UTCTime ->
  Maybe UTCTime ->
  Environment.Flow ApiPayout.PayoutExcludedRes
listPayoutExcluded merchantShortId opCity mbLimit mbOffset mbFrom mbTo = do
  merchant <- QM.findByShortId merchantShortId >>= fromMaybeM (MerchantDoesNotExist merchantShortId.getShortId)
  merchantOpCity <- CQMOC.findByMerchantIdAndCity merchant.id opCity >>= fromMaybeM (InvalidRequest $ "Operating city not found for merchant: " <> show opCity)
  let offset = max 0 (fromMaybe 0 mbOffset)
      limit = resolvePageSize mbLimit
  -- Excluded beneficiaries have a payout_request and no payout_order: nothing was submitted for
  -- them, so there was never an order to carry the exclusion.
  --
  -- Read one past the page for 'hasMore'.
  rows <- QPayoutRequestExtra.findExcludedByMocAndTime merchantOpCity.id.getId mbFrom mbTo (Just (limit + 1)) (Just offset)
  let hasMore = length rows > limit
      requests = take limit rows
  persons <- QPerson.findAllByPersonIds (nub $ map (.beneficiaryId) requests)
  let personsById = Map.fromList [(p.id.getId, p) | p <- persons]
      items = map (toExcludedItem personsById) requests
  pure ApiPayout.PayoutExcludedRes {items = items, hasMore}

toExcludedItem :: Map Text DP.Person -> DPayoutRequest.PayoutRequest -> ApiPayout.PayoutExcludedItem
toExcludedItem personsById request =
  let mbPerson = Map.lookup request.beneficiaryId personsById
   in ApiPayout.PayoutExcludedItem
        { personId = request.beneficiaryId,
          personName = mbPerson <&> (.firstName),
          role = maybe "UNKNOWN" (beneficiaryRoleLabel . (.role)) mbPerson,
          amount = fromMaybe 0 request.amount,
          reason = fromMaybe "NO_PAYOUT_METHOD" request.failureReason,
          excludedAt = request.createdAt,
          -- Which run dropped this person. Without it the same driver excluded on three days is
          -- three indistinguishable rows on the city-wide list.
          payoutRequestId = request.id.getId,
          batchId = (.getId) <$> request.batchId
        }

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
        shortId = (.getShortId) <$> order.shortId,
        payoutRequestId = listToMaybe =<< order.entityIds,
        customerId = order.customerId,
        beneficiaryName = mbPerson <&> (.firstName),
        beneficiaryPhone,
        beneficiaryRole = maybe "UNKNOWN" (beneficiaryRoleLabel . (.role)) mbPerson,
        status = show order.status,
        transferStatus = show <$> order.transferStatus,
        amount = order.amount.amount,
        -- The partner's own text for this item, as it came back on the inquiry, and the
        -- codstatus/rbistatus pair behind it ("E/TXSIP", "E/-"). The pair is the most diagnostic
        -- thing we hold about a stuck order, so it belongs in the view rather than only in the row.
        responseMessage = order.responseMessage,
        responseCode = order.responseCode,
        settlementRef = order.settlementRef,
        settlementRefType = show <$> order.settlementRefType,
        createdAt = order.createdAt,
        updatedAt = order.updatedAt
      }
