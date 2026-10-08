-- Patches the on_confirm order with fulfillment-state, breakup, terms, route-info, fulfillment.type and vehicle.energy_type overrides, since Layer 1 uses fulfillment-state codes outside ONDC's vocabulary and has no notion of BAP/BPP terms.
module Beckn.OnDemand.Transformer.OndcScheduledRide.OnConfirm
  ( ondcScheduledRideOnConfirmMessageBuild,
  )
where

import qualified Beckn.OnDemand.Utils.OndcScheduledRide.Common as OSRCommon
import qualified BecknV2.OnDemand.Types as Spec
import qualified Domain.Types.BapMetadata as DBapMetadata
import qualified Domain.Types.BecknConfig as DBC
import qualified Domain.Types.Booking as DRB
import Kernel.Prelude
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow)

-- | Patches Layer 1's on_confirm order: fulfillment-state, breakup titles, BAP/BPP terms, route info, fulfillment.type, vehicle.energy_type, and echoes back the selected add-ons.
ondcScheduledRideOnConfirmMessageBuild :: (EsqDBFlow m r, CacheFlow m r, MonadFlow m) => DRB.Booking -> Maybe DBapMetadata.BapMetadata -> DBC.BecknConfig -> Spec.ConfirmReqMessage -> m Spec.ConfirmReqMessage
ondcScheduledRideOnConfirmMessageBuild booking mbBapMetadata bppConfig msg = do
  let orderWithBreakupTitles = OSRCommon.overrideOrderBreakupTitles msg.confirmReqMessageOrder
  orderWithOverrides <- OSRCommon.applyOnConfirmOrderOverrides booking.isScheduled booking.transactionId booking.addOnData mbBapMetadata bppConfig orderWithBreakupTitles
  pure msg {Spec.confirmReqMessageOrder = orderWithOverrides}
