module SharedLogic.RSFOrderStatus
  ( computeOrderStatus,
    effectiveClaimedAmount, -- re-export for callers
  )
where

import Kernel.Prelude
import Kernel.Types.Common (HighPrecMoney)
import qualified Lib.Finance.Domain.Types.ReconSettlementOrder as RSO
import SharedLogic.Finance.Reconciliation.Recipes.RsfBapClaimVsPlatformRide (effectiveClaimedAmount)

-- | Live-compute the order-level verdict + diff from the fare and every RSO
-- row for that orderId. Pure. Called by dashboard handlers, the outbound
-- on_receiver_recon payload builder, and CallRSF's pre-send flow. Never
-- writes back to the database.
--
-- diffAmount sign convention (preserved from the existing per-row write
-- path so nothing else needs to change): fare - received.
--   diff == 0  -> PAID
--   diff  > 0  -> UNDERPAID (fare was more than we received)
--   diff  < 0  -> OVERPAID  (received more than the fare)
computeOrderStatus ::
  HighPrecMoney ->
  [RSO.ReconSettlementOrder] ->
  (RSO.OrderReconVerdict, HighPrecMoney)
computeOrderStatus fare rows =
  let received = sum (map effectiveClaimedAmount rows)
      diff = fare - received
      verdict
        | diff == 0 = RSO.PAID
        | diff > 0 = RSO.UNDERPAID
        | otherwise = RSO.OVERPAID
   in (verdict, diff)
