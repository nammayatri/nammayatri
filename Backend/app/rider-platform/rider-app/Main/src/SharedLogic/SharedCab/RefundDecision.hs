-- | R54: what a shared-cab cancel refunds, handed from the cancel guard (`SharedLogic.SharedCab.Cancel`) to
-- `ExternalBPP.Flow.Common.cancel` through a short-lived key, so the bus/metro cancel path stays untouched.
module SharedLogic.SharedCab.RefundDecision
  ( Refund (..),
    isSharedCabBooking,
    refundAmounts,
    refundWithheld,
    cancelRefund,
    setRefundDecision,
    readRefundDecision,
    clearRefundDecision,
  )
where

import qualified BecknV2.FRFS.Enums as Spec
import qualified Domain.Types.FRFSTicketBooking as DFRFSTicketBooking
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Common (HighPrecMoney)
import Kernel.Types.Id
import Kernel.Utils.Common (MonadFlow)
import SharedLogic.FRFSUtils (getServiceTierTypeFromRouteStationsJson)

data Refund = FullRefund | NoRefund
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

isSharedCabBooking :: DFRFSTicketBooking.FRFSTicketBooking -> Bool
isSharedCabBooking booking = getServiceTierTypeFromRouteStationsJson booking.routeStationsJson == Just Spec.SHARED_CAB

-- | (cancellation charges, refund) out of the base fare. There is no partial tier: shared cab has no convenience-fee
-- number yet, so a refund is the whole fare.
refundAmounts :: HighPrecMoney -> Refund -> (HighPrecMoney, HighPrecMoney)
refundAmounts baseFare = \case
  FullRefund -> (0, baseFare)
  NoRefund -> (baseFare, 0)

-- | A cancel that charged the fare and refunds nothing: the payment must stay charged, so it is never marked
-- refund-pending (that mark is what makes the payment service refund the order in full).
refundWithheld :: HighPrecMoney -> HighPrecMoney -> Bool
refundWithheld cancellationCharges refundAmount = cancellationCharges > 0 && refundAmount <= 0

-- | The refund a cancel of `booking` runs with, given whether it is shared cab, whether the rider started it, and the
-- policy decision the guard published. Nothing means "use the bus/metro tier table", which only a booking that is not
-- shared cab ever gets: a rider cancel that skipped the policy guard is refused (Left), a system (technical) cancel
-- refunds in full.
cancelRefund :: Bool -> Bool -> Maybe Refund -> Either () (Maybe Refund)
cancelRefund isSharedCab riderInitiated mbDecision
  | not isSharedCab = Right Nothing
  | Just refund <- mbDecision = Right (Just refund)
  | riderInitiated = Left ()
  | otherwise = Right (Just FullRefund)

refundKey :: Id DFRFSTicketBooking.FRFSTicketBooking -> Text
refundKey bookingId = "sharedcab:refund:" <> bookingId.getId

-- | Written under the booking lock for the length of one cancel; the TTL only collects a crash's leftovers.
setRefundDecision :: (MonadFlow m, Redis.HedisFlow m r) => Id DFRFSTicketBooking.FRFSTicketBooking -> Refund -> m ()
setRefundDecision bookingId refund = Redis.setExp (refundKey bookingId) refund 120

readRefundDecision :: (MonadFlow m, Redis.HedisFlow m r) => Id DFRFSTicketBooking.FRFSTicketBooking -> m (Maybe Refund)
readRefundDecision = Redis.safeGet . refundKey

clearRefundDecision :: (MonadFlow m, Redis.HedisFlow m r) => Id DFRFSTicketBooking.FRFSTicketBooking -> m ()
clearRefundDecision = void . Redis.del . refundKey
