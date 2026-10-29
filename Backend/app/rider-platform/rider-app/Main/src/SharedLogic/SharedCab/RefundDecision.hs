-- | What a shared-cab cancel refunds, handed from the cancel guard to `ExternalBPP.Flow.Common.cancel` via a short-lived key.
module SharedLogic.SharedCab.RefundDecision
  ( Refund (..),
    isSharedCabBooking,
    gateByPayment,
    gateNoPayment,
    owesRefund,
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
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow)
import SharedLogic.FRFSUtils (getServiceTierTypeFromRouteStationsJson)
import SharedLogic.SharedCab.Keys (refundKey)
import qualified Storage.Queries.FRFSTicketBookingPayment as QFRFSTicketBookingPayment

data Refund = FullRefund | NoRefund | NothingPaid
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

isSharedCabBooking :: DFRFSTicketBooking.FRFSTicketBooking -> Bool
isSharedCabBooking booking = getServiceTierTypeFromRouteStationsJson booking.routeStationsJson == Just Spec.SHARED_CAB

refundAmounts :: HighPrecMoney -> Refund -> (HighPrecMoney, HighPrecMoney)
refundAmounts baseFare = \case
  FullRefund -> (0, baseFare)
  NoRefund -> (baseFare, 0)
  NothingPaid -> (0, 0)

-- | A booking with no payment row was never charged in-app. Keyed on the payment, not the tier's current config, so a paid booking keeps its refund.
gateNoPayment :: Bool -> Refund -> Refund
gateNoPayment hasPayment refund = if hasPayment then refund else NothingPaid

gateByPayment :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => DFRFSTicketBooking.FRFSTicketBooking -> Refund -> m Refund
gateByPayment booking refund = (\mbPayment -> gateNoPayment (isJust mbPayment) refund) <$> QFRFSTicketBookingPayment.findTicketBookingPayment booking

owesRefund :: Refund -> Bool
owesRefund = (== FullRefund)

-- | A cancel that charged the fare and refunds nothing: never marked refund-pending (that mark makes the payment service refund in full).
refundWithheld :: HighPrecMoney -> HighPrecMoney -> Bool
refundWithheld cancellationCharges refundAmount = cancellationCharges > 0 && refundAmount <= 0

-- | Nothing means "use the bus/metro tier table"; a rider cancel that skipped the policy guard is refused (Left), a system cancel refunds in full.
cancelRefund :: Bool -> Bool -> Maybe Refund -> Either () (Maybe Refund)
cancelRefund isSharedCab riderInitiated mbDecision
  | not isSharedCab = Right Nothing
  | Just refund <- mbDecision = Right (Just refund)
  | riderInitiated = Left ()
  | otherwise = Right (Just FullRefund)

setRefundDecision :: (MonadFlow m, Redis.HedisFlow m r) => Id DFRFSTicketBooking.FRFSTicketBooking -> Refund -> m ()
setRefundDecision bookingId refund = Redis.setExp (refundKey bookingId) refund 120

readRefundDecision :: (MonadFlow m, Redis.HedisFlow m r) => Id DFRFSTicketBooking.FRFSTicketBooking -> m (Maybe Refund)
readRefundDecision = Redis.safeGet . refundKey

clearRefundDecision :: (MonadFlow m, Redis.HedisFlow m r) => Id DFRFSTicketBooking.FRFSTicketBooking -> m ()
clearRefundDecision = void . Redis.del . refundKey
