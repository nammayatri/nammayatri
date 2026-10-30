{-
  Lib.Finance.Transfer.History

  Unified account history: every kind of movement (internal, outbound, inbound) through
  one feed of Transfer rows, plus the settled-ledger read keyed by settlement id.
  Both reads filter, sort and page at the DB — an account's transfer history grows
  without bound, so nothing here may fetch it whole.
-}
module Lib.Finance.Transfer.History
  ( getAccountHistory,
    getSettledLedgerBySettlement,
  )
where

import Kernel.Prelude
import Kernel.Types.Id (Id)
import Lib.Finance.Domain.Types.Account (Account)
import Lib.Finance.Domain.Types.LedgerEntry (LedgerEntry)
import qualified Lib.Finance.Domain.Types.Transfer as DT
import Lib.Finance.Ledger.Service (findBySettlementIdWithOptions)
import qualified Lib.Finance.Storage.Beam.BeamFlow as BeamFlow
import qualified Lib.Finance.Storage.Queries.TransferExtra as QTE

-- | Transfers touching an account, newest first.
getAccountHistory ::
  (BeamFlow.BeamFlow m r) =>
  Id Account ->
  Maybe DT.TransferDirection ->
  Int -> -- offset
  Int -> -- limit
  m [DT.Transfer]
getAccountHistory accountId mbDirection offset limit =
  QTE.findByAccountWithOptions accountId mbDirection (Just limit) (Just offset)

-- | The settled ledger entries one settlement (transfer) covered, filtered to a
--   reference type: "which earnings did this payout pay out".
getSettledLedgerBySettlement ::
  (BeamFlow.BeamFlow m r) =>
  Text -> -- reference type
  Text -> -- settlement id (= the domain process id for kernel-driven payouts)
  Int -> -- offset
  Int -> -- limit
  m [LedgerEntry]
getSettledLedgerBySettlement refType settlementId offset limit =
  findBySettlementIdWithOptions refType settlementId (Just limit) (Just offset)
