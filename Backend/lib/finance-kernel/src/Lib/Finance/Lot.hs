{-
  Lib.Finance.Lot

  Expiring lots on a ledger account. A lot is a credit to the account (a LiabilityCreated entry, expiresAt
  = when it lapses, Nothing = never). Spending a lot, or letting it lapse, is a further entry out of the
  account that names the lot in entityReferenceType = ExpiringLot / entityReferenceId = the lot's id. The
  ledger stays append-only: what is left of a lot is always its amount minus the entries naming it.

  This module only reads lots and decides how an amount is split across them (earliest expiry first, then
  oldest; lots that never expire last). Posting the spending entries is the caller's, through
  'Lib.Finance.Ledger.Service.createEntryWithBalanceUpdate' under its own owner lock, so the domain picks the
  counter-account and reference types.
-}
module Lib.Finance.Lot
  ( LotView (..),
    openLots,
    lapsedLots,
    partitionByExpiry,
    allocateInExpiryOrder,
    retireSpentLots,
  )
where

import Data.List (sortOn)
import qualified Data.Map.Strict as Map
import Kernel.Prelude
import Kernel.Types.Common (HighPrecMoney)
import Kernel.Types.Id (Id (..))
import Lib.Finance.Core.Types (HasActorInfo)
import Lib.Finance.Domain.Types.Account (Account)
import Lib.Finance.Domain.Types.LedgerEntry (LedgerEntry (..))
import Lib.Finance.Ledger.Service (markEntriesAsClaimed)
import qualified Lib.Finance.Storage.Beam.BeamFlow as BeamFlow
import qualified Lib.Finance.Storage.Queries.LedgerEntryExtra as QLedgerExtra

-- | A lot with what is left of it.
data LotView = LotView
  { lot :: LedgerEntry,
    remaining :: HighPrecMoney
  }
  deriving (Generic)

-- | Every lot of the account that has not been retired ('retireSpentLots'), live or lapsed, with its
--   remaining amount. Lots with nothing left that were not retired yet are included with remaining <= 0.
openLots ::
  (BeamFlow.BeamFlow m r) =>
  Id Account ->
  m [LotView]
openLots accountId = do
  lots <- QLedgerExtra.findOpenLotsByToAccount accountId
  consumptions <- QLedgerExtra.findLotConsumptions (map (.id.getId) lots)
  let spentByLot = Map.fromListWith (+) [(lotId, e.amount) | e <- consumptions, Just lotId <- [e.entityReferenceId]]
  pure [LotView {lot = l, remaining = l.amount - Map.findWithDefault 0 l.id.getId spentByLot} | l <- lots]

-- | The lots that lapsed before the given instant and still have something left. Reads only those, so it
--   stays small whatever the account's history; 'openLots' reads every lot not yet retired.
lapsedLots ::
  (BeamFlow.BeamFlow m r) =>
  Id Account ->
  UTCTime ->
  m [LotView]
lapsedLots accountId now = do
  lots <- QLedgerExtra.findLapsedOpenLotsByToAccount accountId now
  consumptions <- QLedgerExtra.findLotConsumptions (map (.id.getId) lots)
  let spentByLot = Map.fromListWith (+) [(lotId, e.amount) | e <- consumptions, Just lotId <- [e.entityReferenceId]]
  pure [v | l <- lots, let v = LotView {lot = l, remaining = l.amount - Map.findWithDefault 0 l.id.getId spentByLot}, v.remaining > 0]

-- | (live, lapsed) at the given instant. Live lots with something left come back in spending order:
--   earliest expiry first, then oldest, lots that never expire last. Lapsed lots are those with an
--   expiresAt before the instant and something left to expire.
partitionByExpiry :: UTCTime -> [LotView] -> ([LotView], [LotView])
partitionByExpiry now views =
  (sortOn spendingOrder live, lapsed)
  where
    withRemaining = filter (\v -> v.remaining > 0) views
    isLapsed v = maybe False (< now) v.lot.expiresAt
    lapsed = filter isLapsed withRemaining
    live = filter (not . isLapsed) withRemaining
    spendingOrder v = (isNothing v.lot.expiresAt, v.lot.expiresAt, v.lot.timestamp)

-- | Split an amount over lots already in spending order. Returns the part taken from each lot and what the
--   lots could not cover.
allocateInExpiryOrder :: HighPrecMoney -> [LotView] -> ([(LotView, HighPrecMoney)], HighPrecMoney)
allocateInExpiryOrder = go []
  where
    go acc toTake [] = (reverse acc, max 0 toTake)
    go acc toTake (v : vs)
      | toTake <= 0 = (reverse acc, 0)
      | otherwise =
        let taken = min toTake v.remaining
         in go ((v, taken) : acc) (toTake - taken) vs

-- | Mark lots with nothing left as settled, so 'openLots' stops returning them. Call after posting spending
--   or expiry entries; recomputes from the ledger, so it is safe to repeat.
retireSpentLots ::
  (BeamFlow.BeamFlow m r, HasActorInfo m r) =>
  Id Account ->
  m ()
retireSpentLots accountId = do
  views <- openLots accountId
  markEntriesAsClaimed [v.lot.id | v <- views, v.remaining <= 0]
