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
import Data.Time (Day, UTCTime (..), addDays)
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Person as DP
import qualified Environment
import Kernel.External.Encryption (decrypt)
import Kernel.Prelude
import qualified Kernel.Types.Beckn.Context
import Kernel.Types.Error
import qualified Kernel.Types.Id as Id
import Kernel.Utils.Common
import Lib.ConfigPilot.Interface.Types (getOneConfig)
import qualified Lib.Payment.Domain.Types.PayoutBatch as DPayoutBatch
import qualified Lib.Payment.Domain.Types.PayoutOrder as DPayoutOrder
import qualified Lib.Payment.Domain.Types.PayoutRequest as DPayoutRequest
import qualified Lib.Payment.Storage.Queries.PayoutBatch as QPayoutBatch
import qualified Lib.Payment.Storage.Queries.PayoutBatchExtra as QPayoutBatchExtra
import qualified Lib.Payment.Storage.Queries.PayoutOrderExtra as QPayoutOrderExtra
import qualified Lib.Payment.Storage.Queries.PayoutRequestExtra as QPayoutRequestExtra
import qualified Storage.CachedQueries.Merchant as QM
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import Storage.ConfigPilot.Config.TransporterConfig (TransporterConfigDimensions (..))
import qualified Storage.Queries.Person as QPerson

-- | Cap on one page of any payout list, so a single request cannot force an unbounded read.
--   Safe because 'hasMore' tells the caller the page was cut short.
maxPageSize :: Int
maxPageSize = 1000

-- | Page size capped at 'maxPageSize'. At least 1, or a client paging on 'hasMore' would fetch the
--   same empty page forever.
resolvePageSize :: Maybe Int -> Int
resolvePageSize mbLimit = min maxPageSize (max 1 (fromMaybe defaultPageSize mbLimit))

defaultPageSize :: Int
defaultPageSize = 20

-- | Paginated payout_batch list, filtered by execution date (the day the bank runs the batch), inclusive.
listPayoutBatches ::
  Id.ShortId DM.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Maybe Int ->
  Maybe Int ->
  Day -> -- execution date from
  Day -> -- execution date to
  Maybe DPayoutBatch.PayoutBatchStatus ->
  Maybe DPayoutBatch.PayoutBatchOrigin ->
  Maybe DPayoutBatch.PayoutBatchRail ->
  Environment.Flow ApiPayout.PayoutBatchListRes
listPayoutBatches merchantShortId opCity mbLimit mbOffset fromDate toDate mbStatus mbOrigin mbRail = do
  when (toDate < fromDate) $ throwError (InvalidRequest "toDate must not be before fromDate")
  merchant <- QM.findByShortId merchantShortId >>= fromMaybeM (MerchantDoesNotExist merchantShortId.getShortId)
  merchantOpCity <- CQMOC.findByMerchantIdAndCity merchant.id opCity >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchant-Id-" <> merchant.id.getId <> "-city-" <> show opCity)
  let limit = resolvePageSize mbLimit
      offset = max 0 (fromMaybe 0 mbOffset)
  -- One extra row tells us there is a next page without counting every match.
  rows <-
    QPayoutBatchExtra.findAllPayoutBatchesWithFilters
      merchantOpCity.id.getId
      fromDate
      toDate
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
      -- Partner code (or ours, e.g. CALL_FAILED) in its own column so batches can be searched by it.
      failureCode = batch.failureCode,
      statusCheckRound = batch.statusCheckRound,
      nextStatusCallAt = batch.nextStatusCallAt,
      statusNoDataReplies = batch.statusNoDataReplies,
      submittedAt = batch.submittedAt,
      resolvedAt = batch.resolvedAt,
      createdAt = batch.createdAt,
      updatedAt = batch.updatedAt
    }

-- | Driver or fleet-owner label; the only role split the payout views need.
beneficiaryRoleLabel :: DP.Role -> Text
beneficiaryRoleLabel role
  | role `elem` [DP.FLEET_OWNER, DP.FLEET_BUSINESS] = "FLEET_OWNER"
  | otherwise = "DRIVER"

-- | Every payout_order in one batch, with beneficiary details for adhoc retry, plus everyone it excluded.
listPayoutBatchOrders ::
  Id.ShortId DM.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Text ->
  Environment.Flow ApiPayout.PayoutBatchOrdersRes
listPayoutBatchOrders merchantShortId opCity batchId = do
  merchant <- QM.findByShortId merchantShortId >>= fromMaybeM (MerchantDoesNotExist merchantShortId.getShortId)
  merchantOpCity <- CQMOC.findByMerchantIdAndCity merchant.id opCity >>= fromMaybeM (MerchantOperatingCityNotFound $ "merchant-Id-" <> merchant.id.getId <> "-city-" <> show opCity)
  batch <- QPayoutBatch.findByPrimaryKey (Id.Id batchId) >>= fromMaybeM (InvalidRequest $ "PayoutBatch not found: " <> batchId)
  -- Stop one city from reading another city's batch.
  unless (batch.merchantId == merchant.id.getId && batch.merchantOperatingCityId == merchantOpCity.id.getId) $
    throwError (InvalidRequest $ "PayoutBatch not found: " <> batchId)
  -- Not paginated: a batch is already capped by the partner's maxItemsPerBatch.
  orders <- QPayoutOrderExtra.findAllByBatchIdWithOptions batch.id.getId Nothing Nothing
  persons <- QPerson.findAllByPersonIds (nub $ map (.customerId) orders)
  let personsById = Map.fromList [(p.id.getId, p) | p <- persons]
  orderItems <- mapM (toOrderListItem personsById) orders
  -- Excluded people have no payout_order, so read them from payout_request, and only when there are any.
  excludedItems <-
    if batch.excludedCount > 0
      then do
        excludedReqs <- QPayoutRequestExtra.findExcludedOfBatch batch
        exPersons <- QPerson.findAllByPersonIds (nub $ map (.beneficiaryId) excludedReqs)
        let exById = Map.fromList [(p.id.getId, p) | p <- exPersons]
        pure $ map (toExcludedRequestItem exById) excludedReqs
      else pure []
  pure ApiPayout.PayoutBatchOrdersRes {orders = orderItems, excluded = excludedItems}

-- | An excluded beneficiary for the batch view; same shape as toExcludedItem for the city list.
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

-- | Beneficiaries excluded for lack of a usable payout method, over an inclusive range of the city's
--   local days. An admin can pay them with adhoc initiate once their bank details are fixed.
listPayoutExcluded ::
  Id.ShortId DM.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Maybe Int ->
  Maybe Int ->
  Maybe Day ->
  Maybe Day ->
  Environment.Flow ApiPayout.PayoutExcludedRes
listPayoutExcluded merchantShortId opCity mbLimit mbOffset mbFromDate mbToDate = do
  whenJust ((,) <$> mbFromDate <*> mbToDate) $ \(fromDate, toDate) ->
    when (toDate < fromDate) $ throwError (InvalidRequest "to must not be before from")
  merchant <- QM.findByShortId merchantShortId >>= fromMaybeM (MerchantDoesNotExist merchantShortId.getShortId)
  merchantOpCity <- CQMOC.findByMerchantIdAndCity merchant.id opCity >>= fromMaybeM (InvalidRequest $ "Operating city not found for merchant: " <> show opCity)
  transporterConfig <- getOneConfig (TransporterConfigDimensions {merchantOperatingCityId = merchantOpCity.id.getId}) Nothing >>= fromMaybeM (TransporterConfigNotFound merchantOpCity.id.getId)
  let offset = max 0 (fromMaybe 0 mbOffset)
      limit = resolvePageSize mbLimit
      -- Local midnight in UTC; the range is [start of from, start of the day after to).
      localDayStart day = addUTCTime (negate (secondsToNominalDiffTime transporterConfig.timeDiffFromUtc)) (UTCTime day 0)
      mbFrom = localDayStart <$> mbFromDate
      mbTo = localDayStart . addDays 1 <$> mbToDate
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
          -- Tells apart the same driver excluded on different days.
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
        -- Partner text plus the codstatus/rbistatus pair (e.g. "E/TXSIP"): the best clue for a stuck order.
        responseMessage = order.responseMessage,
        responseCode = order.responseCode,
        settlementRef = order.settlementRef,
        settlementRefType = show <$> order.settlementRefType,
        createdAt = order.createdAt,
        updatedAt = order.updatedAt
      }
