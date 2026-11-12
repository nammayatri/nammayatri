module Storage.Queries.RsfLedgerAggregates
  ( sumAllocatedByOrder,
    sumByUtrAndEntryType,
    findUnclaimedCompletedBookings,
    findUnclaimedCompletedBookingsPage,
  )
where

import qualified Data.HashSet as HS
import Data.List (sortOn)
import qualified Data.Map.Strict as Map
import Data.Ord (Down (..))
import qualified Domain.Types.Booking as DBooking
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.RsfReconLedgerEntry as DL
import Kernel.Prelude
import Kernel.Types.Common (HighPrecMoney)
import Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, addUTCTime, getCurrentTime)
import Lib.Finance.Storage.Beam.BeamFlow (BeamFlow)
import qualified Storage.Queries.Booking as QBooking
import qualified Storage.Queries.RsfReconLedgerEntry as QLedger

-- RSF reads that combine several rows, all through the KV read path, so they see rows written moments ago that the
-- drainer has not moved yet: allocation sums, the missing-rides send and the dashboard's missed-orders page.

-- Σ BANK_ALLOCATION per order (the ledger's order_id secondary key). Allocation decides new writes from it.
sumAllocatedByOrder :: (BeamFlow m r) => Id DM.Merchant -> [Text] -> m (Map.Map Text HighPrecMoney)
sumAllocatedByOrder _ [] = pure Map.empty
sumAllocatedByOrder merchantId orderIds = do
  rows <- QLedger.findAllByMerchantEntryTypeAndOrderIds merchantId DL.BANK_ALLOCATION orderIds
  pure $ Map.fromListWith (+) [(o, e.amount) | e <- rows, Just o <- [e.orderId]]

-- Σ amount per (UTR, entry type) (the ledger's utr secondary key).
sumByUtrAndEntryType :: (BeamFlow m r) => Id DM.Merchant -> [DL.RsfLedgerEntryType] -> [Text] -> m (Map.Map (Text, DL.RsfLedgerEntryType) HighPrecMoney)
sumByUtrAndEntryType _ _ [] = pure Map.empty
sumByUtrAndEntryType merchantId entryTypes utrs = do
  rows <- QLedger.findAllByMerchantEntryTypesAndUtrs merchantId entryTypes utrs
  pure $ Map.fromListWith (+) [((u, e.entryType), e.amount) | e <- rows, Just u <- [e.utr]]

-- Completed online bookings created in the range that no receiver_recon ever claimed (no BAP_CLAIM row with the
-- booking id as order id), newest first.
findUnclaimedCompletedBookings :: (BeamFlow m r, MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id DM.Merchant -> UTCTime -> UTCTime -> m [DBooking.Booking]
findUnclaimedCompletedBookings merchantId from to = do
  bookings <- QBooking.findAllCompletedOnlineByProviderAndCreatedAtRange merchantId from to
  claims <- QLedger.findAllByMerchantEntryTypeAndOrderIds merchantId DL.BAP_CLAIM (map (.id.getId) bookings)
  let claimed = HS.fromList (mapMaybe (.orderId) claims)
  pure $ sortOn (Down . (.createdAt)) [b | b <- bookings, not (HS.member b.id.getId claimed)]

-- The dashboard's missed-orders page, paged by the query: the order ids claimed since `from` (a booking created in
-- the range can only be claimed after it was created) are read from the ledger through KV, and the booking query
-- leaves them out and applies the BAP filter, limit and offset. Returns (total, page).
findUnclaimedCompletedBookingsPage :: (BeamFlow m r, MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id DM.Merchant -> UTCTime -> UTCTime -> Maybe Text -> Int -> Int -> m (Int, [DBooking.Booking])
findUnclaimedCompletedBookingsPage merchantId from to mbBapId limit offset = do
  now <- getCurrentTime
  claims <- QLedger.findAllByMerchantEntryTypeAndCreatedAtRange merchantId DL.BAP_CLAIM from (addUTCTime 60 now)
  let claimed = map Id . HS.toList $ HS.fromList (mapMaybe (.orderId) claims)
  total <- QBooking.countCompletedOnlineByProviderAndCreatedAtRange merchantId from to mbBapId claimed
  page <- QBooking.findCompletedOnlinePageByProviderAndCreatedAtRange merchantId from to mbBapId claimed limit offset
  pure (total, page)
