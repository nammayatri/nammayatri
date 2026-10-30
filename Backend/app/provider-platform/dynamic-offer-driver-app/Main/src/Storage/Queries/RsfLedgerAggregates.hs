module Storage.Queries.RsfLedgerAggregates
  ( sumAllocatedByOrder,
    sumByUtrAndEntryType,
    findUnclaimedCompletedBookings,
    findUnclaimedCompletedBookingsPage,
    countUnclaimedCompletedBookings,
  )
where

import qualified Data.HashSet as HS
import Data.List (sortOn)
import qualified Data.Map.Strict as Map
import Data.Ord (Down (..))
import qualified Database.Beam as B
import Database.Beam.Postgres (Postgres)
import qualified Domain.Types.Booking as DBooking
import qualified Domain.Types.Extra.MerchantPaymentMethod as DMPM
import qualified EulerHS.Language as L
import Kernel.Beam.Functions (FromTType' (fromTType'), getReplicaBeamConfig)
import Kernel.Prelude
import Kernel.Types.Common (HighPrecMoney)
import Kernel.Types.Error (GenericError (InternalError))
import Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, throwError)
import qualified Lib.Finance.Domain.Types.RsfReconLedgerEntry as DL
import Lib.Finance.Storage.Beam.BeamFlow (BeamFlow)
import qualified Lib.Finance.Storage.Queries.RsfReconLedgerEntry as QLedger
import qualified Storage.Beam.Booking as BeamBooking
import qualified Storage.Beam.Common as BeamCommon
import qualified Storage.Queries.Booking as QBooking
import Storage.Queries.OrphanInstances.Booking ()

-- RSF reads that combine several rows. Those that drive a write (allocation sums, the missing-rides send) go through
-- the KV read path, so they see rows written moments ago that the drainer has not moved yet. The dashboard's
-- unclaimed-rides page filters, pages and counts in the database.

-- Σ BANK_ALLOCATION per order (the ledger's order_id secondary key). Allocation decides new writes from it.
sumAllocatedByOrder :: (BeamFlow m r) => Text -> [Text] -> m (Map.Map Text HighPrecMoney)
sumAllocatedByOrder _ [] = pure Map.empty
sumAllocatedByOrder merchantId orderIds = do
  rows <- QLedger.findAllByMerchantEntryTypeAndOrderIds merchantId DL.BANK_ALLOCATION orderIds
  pure $ Map.fromListWith (+) [(o, e.amount) | e <- rows, Just o <- [e.orderId]]

-- Σ amount per (UTR, entry type) (the ledger's utr secondary key).
sumByUtrAndEntryType :: (BeamFlow m r) => Text -> [DL.RsfLedgerEntryType] -> [Text] -> m (Map.Map (Text, DL.RsfLedgerEntryType) HighPrecMoney)
sumByUtrAndEntryType _ _ [] = pure Map.empty
sumByUtrAndEntryType merchantId entryTypes utrs = do
  rows <- QLedger.findAllByMerchantEntryTypesAndUtrs merchantId entryTypes utrs
  pure $ Map.fromListWith (+) [((u, e.entryType), e.amount) | e <- rows, Just u <- [e.utr]]

-- Completed online bookings created in the range that no receiver_recon ever claimed (no BAP_CLAIM row with the
-- booking id as order id), newest first.
findUnclaimedCompletedBookings :: (BeamFlow m r, MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Text -> UTCTime -> UTCTime -> m [DBooking.Booking]
findUnclaimedCompletedBookings merchantId from to = do
  bookings <- QBooking.findAllCompletedOnlineByProviderAndCreatedAtRange (Id merchantId) from to
  claims <- QLedger.findAllByMerchantEntryTypeAndOrderIds merchantId DL.BAP_CLAIM (map (.id.getId) bookings)
  let claimed = HS.fromList (mapMaybe (.orderId) claims)
  pure $ sortOn (Down . (.createdAt)) [b | b <- bookings, not (HS.member b.id.getId claimed)]

-- Dashboard page of the same rides, filtered, ordered and paged in the database (replica):
-- SELECT b.* FROM booking b
--   LEFT JOIN rsf_recon_ledger_entry e ON e.merchant_id = ? AND e.entry_type = 'BAP_CLAIM' AND e.order_id = b.id
-- WHERE b.provider_id = ? AND b.status = 'COMPLETED' AND b.payment_instrument <> 'Cash'
--   AND b.created_at >= ? AND b.created_at < ? AND e.id IS NULL
-- ORDER BY b.created_at DESC LIMIT n OFFSET m
findUnclaimedCompletedBookingsPage :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Text -> UTCTime -> UTCTime -> Int -> Int -> m [DBooking.Booking]
findUnclaimedCompletedBookingsPage merchantId from to limit offset = do
  dbConf <- getReplicaBeamConfig
  res <-
    L.runDB dbConf $
      L.findRows $
        B.select $
          B.limit_ (fromIntegral limit) $
            B.offset_ (fromIntegral offset) $
              B.orderBy_ (\b -> B.desc_ b.createdAt) $
                unclaimedCompletedBookings merchantId from to
  rows <- orThrow "findUnclaimedCompletedBookingsPage" res
  catMaybes <$> mapM fromTType' rows

-- SELECT COUNT(*) over the same anti-join as the page.
countUnclaimedCompletedBookings :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Text -> UTCTime -> UTCTime -> m Int
countUnclaimedCompletedBookings merchantId from to = do
  dbConf <- getReplicaBeamConfig
  res <- L.runDB dbConf $ L.findRow $ B.select $ B.aggregate_ (\_ -> B.as_ @Int B.countAll_) (unclaimedCompletedBookings merchantId from to)
  fromMaybe 0 <$> orThrow "countUnclaimedCompletedBookings" res

unclaimedCompletedBookings :: Text -> UTCTime -> UTCTime -> B.Q Postgres BeamCommon.AtlasDB s (BeamBooking.BookingT (B.QExpr Postgres s))
unclaimedCompletedBookings merchantId from to =
  fmap fst $
    B.filter_ (\(_, e) -> B.isNothing_ e.id) $ do
      b <-
        B.filter_
          ( \b ->
              b.providerId B.==. B.val_ merchantId
                B.&&. b.status B.==. B.val_ DBooking.COMPLETED
                B.&&. b.paymentInstrument B./=. B.val_ (Just DMPM.Cash)
                B.&&. b.createdAt B.>=. B.val_ from
                B.&&. b.createdAt B.<. B.val_ to
          )
          (B.all_ (BeamCommon.booking BeamCommon.atlasDB))
      e <-
        B.leftJoin_
          (B.all_ (BeamCommon.rsfReconLedgerEntry BeamCommon.atlasDB))
          (\e -> e.merchantId B.==. B.val_ merchantId B.&&. e.entryType B.==. B.val_ DL.BAP_CLAIM B.&&. e.orderId B.==. B.just_ b.id)
      pure (b, e)

-- A failed query is an error, never "no rows": an empty result is a valid answer, a failure is not.
orThrow :: (MonadFlow m, Show e) => Text -> Either e a -> m a
orThrow what = either (\e -> throwError (InternalError (what <> ": " <> show e))) pure
