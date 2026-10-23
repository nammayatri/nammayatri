{-
  Lib.Finance.Transfer.History

  Unified account history: every kind of movement (internal, outbound, inbound) through
  one feed of Transfer rows, plus the settled-ledger read keyed by settlement id.
-}
module Lib.Finance.Transfer.History
  ( getAccountHistory,
    getSettledLedgerBySettlement,
  )
where

import Data.List (sortBy)
import Data.Ord (Down (..), comparing)
import Kernel.Prelude
import Kernel.Types.Id (Id)
import Lib.Finance.Domain.Types.Account (Account)
import Lib.Finance.Domain.Types.LedgerEntry (LedgerEntry)
import qualified Lib.Finance.Domain.Types.Transfer as DT
import Lib.Finance.Ledger.Service (findBySettlementId)
import qualified Lib.Finance.Storage.Beam.BeamFlow as BeamFlow
import qualified Lib.Finance.Storage.Queries.Transfer as QT

-- | Transfers touching an account, newest first. Per-account row counts are small
--   enough that in-memory paging is fine; revisit with a DB-side page if that changes.
getAccountHistory ::
  (BeamFlow.BeamFlow m r) =>
  Id Account ->
  Maybe DT.TransferDirection ->
  Int -> -- offset
  Int -> -- limit
  m [DT.Transfer]
getAccountHistory accountId mbDirection offset limit = do
  rows <- QT.findAllByAccountId accountId
  pure
    . take limit
    . drop offset
    . sortBy (comparing (Down . (.createdAt)))
    $ filter (\t -> maybe True (== t.direction) mbDirection) rows

-- | The settled ledger entries one settlement (transfer) covered, filtered to a
--   reference type: "which earnings did this payout pay out".
getSettledLedgerBySettlement ::
  (BeamFlow.BeamFlow m r) =>
  Text -> -- reference type
  Text -> -- settlement id (= the Transfer id for kernel-driven payouts)
  Int -> -- offset
  Int -> -- limit
  m [LedgerEntry]
getSettledLedgerBySettlement refType settlementId offset limit = do
  entries <- findBySettlementId settlementId
  pure
    . take limit
    . drop offset
    . sortBy (comparing (Down . (.timestamp)))
    $ filter (\e -> e.referenceType == refType) entries
