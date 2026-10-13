module Storage.Queries.RsfLedgerAggregates
  ( sumAllocatedByOrder,
    sumByUtrAndEntryType,
    findUnclaimedCompletedBookingsPage,
  )
where

import qualified Data.Map.Strict as Map
import qualified Database.Beam as B
import Database.Beam.Postgres (Postgres)
import qualified Domain.Types.Booking as DBooking
import qualified Domain.Types.Extra.MerchantPaymentMethod as DMPM
import qualified EulerHS.Language as L
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Common (HighPrecMoney)
import Kernel.Types.Error (GenericError (InternalError))
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, throwError)
import qualified Lib.Finance.Domain.Types.RsfReconLedgerEntry as DL
import qualified Lib.Finance.Storage.Beam.RsfReconLedgerEntry as Beam
import qualified Storage.Beam.Common as BeamCommon
import Storage.Queries.OrphanInstances.Booking ()

-- Ledger sums computed in the database for auto-allocation.

type DbFlow m r = (MonadFlow m, EsqDBFlow m r, CacheFlow m r)

ledger :: B.Q Postgres BeamCommon.AtlasDB s (Beam.RsfReconLedgerEntryT (B.QExpr Postgres s))
ledger = B.all_ (BeamCommon.rsfReconLedgerEntry BeamCommon.atlasDB)

-- A failed query is an error, never "no rows": an empty result is a valid answer, a failure is not.
orThrow :: (MonadFlow m, Show e) => Text -> Either e a -> m a
orThrow what = either (\e -> throwError (InternalError (what <> ": " <> show e))) pure

-- SELECT order_id, SUM(amount) FROM ledger WHERE entry_type = BANK_ALLOCATION AND order_id IN (...) GROUP BY order_id
sumAllocatedByOrder :: (DbFlow m r) => Text -> [Text] -> m (Map.Map Text HighPrecMoney)
sumAllocatedByOrder _ [] = pure Map.empty
sumAllocatedByOrder merchantId orderIds = do
  dbConf <- getReplicaBeamConfig
  res <-
    L.runDB dbConf $
      L.findRows $
        B.select $
          B.aggregate_ (\e -> (B.group_ e.orderId, B.as_ @(Maybe HighPrecMoney) (B.sum_ e.amount))) $
            B.filter_'
              ( \e ->
                  B.sqlBool_ (e.merchantId B.==. B.val_ merchantId)
                    B.&&?. B.sqlBool_ (e.entryType B.==. B.val_ DL.BANK_ALLOCATION)
                    B.&&?. B.sqlBool_ (e.orderId `B.in_` map (B.val_ . Just) orderIds)
              )
              ledger
  rows <- orThrow "sumAllocatedByOrder" res
  pure $ Map.fromList [(o, fromMaybe 0 total) | (Just o, total) <- rows]

-- SELECT utr, entry_type, SUM(amount) FROM ledger WHERE utr IN (...) AND entry_type IN (...) GROUP BY utr, entry_type
sumByUtrAndEntryType :: (DbFlow m r) => Text -> [DL.RsfLedgerEntryType] -> [Text] -> m (Map.Map (Text, DL.RsfLedgerEntryType) HighPrecMoney)
sumByUtrAndEntryType _ _ [] = pure Map.empty
sumByUtrAndEntryType merchantId entryTypes utrs = do
  dbConf <- getReplicaBeamConfig
  res <-
    L.runDB dbConf $
      L.findRows $
        B.select $
          B.aggregate_ (\e -> (B.group_ e.utr, B.group_ e.entryType, B.as_ @(Maybe HighPrecMoney) (B.sum_ e.amount))) $
            B.filter_'
              ( \e ->
                  B.sqlBool_ (e.merchantId B.==. B.val_ merchantId)
                    B.&&?. B.sqlBool_ (e.entryType `B.in_` map B.val_ entryTypes)
                    B.&&?. B.sqlBool_ (e.utr `B.in_` map (B.val_ . Just) utrs)
              )
              ledger
  rows <- orThrow "sumByUtrAndEntryType" res
  pure $ Map.fromList [((u, t), fromMaybe 0 total) | (Just u, t, total) <- rows]

-- Completed bookings in the range that the BAP collected (payment instrument is not Cash) and that no
-- receiver_recon ever claimed, newest first, one page. Anti-join: LEFT JOIN the ledger's BAP_CLAIM
-- rows on booking id and keep the bookings with no match.
-- SELECT b.* FROM booking b
--   LEFT JOIN ledger e ON e.merchant_id = ? AND e.entry_type = BAP_CLAIM AND e.order_id = b.id
-- WHERE b.provider_id = ? AND b.status = COMPLETED AND b.payment_instrument <> 'Cash'
--   AND b.created_at >= ? AND b.created_at < ? AND e.id IS NULL
-- ORDER BY b.created_at DESC LIMIT n OFFSET m
findUnclaimedCompletedBookingsPage :: (DbFlow m r) => Text -> UTCTime -> UTCTime -> Int -> Int -> m [DBooking.Booking]
findUnclaimedCompletedBookingsPage merchantId from to limit offset = do
  dbConf <- getReplicaBeamConfig
  res <-
    L.runDB dbConf $
      L.findRows $
        B.select $
          B.limit_ (fromIntegral limit) $
            B.offset_ (fromIntegral offset) $
              B.orderBy_ (\b -> B.desc_ b.createdAt) $
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
                        ledger
                        (\e -> e.merchantId B.==. B.val_ merchantId B.&&. e.entryType B.==. B.val_ DL.BAP_CLAIM B.&&. e.orderId B.==. B.just_ b.id)
                    pure (b, e)
  rows <- orThrow "findUnclaimedCompletedBookingsPage" res
  catMaybes <$> mapM fromTType' rows
