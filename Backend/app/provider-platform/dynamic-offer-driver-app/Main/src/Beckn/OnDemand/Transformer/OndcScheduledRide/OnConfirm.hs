-- Patches the on_confirm order with fulfillment-state, breakup, terms, route-info, fulfillment.type and vehicle.energy_type overrides, since Layer 1 uses fulfillment-state codes outside ONDC's vocabulary and has no notion of BAP/BPP terms.
module Beckn.OnDemand.Transformer.OndcScheduledRide.OnConfirm
  ( ondcScheduledRideOnConfirmMessageBuild,
  )
where

import qualified Beckn.OnDemand.Utils.OndcScheduledRide.Common as OSRCommon
import qualified BecknV2.OnDemand.Types as Spec
import qualified Domain.Types.BecknConfig as DBC
import qualified Domain.Types.Booking as DRB
import Kernel.Prelude
import qualified Kernel.Types.Beckn.Domain as Domain
import Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow)
import qualified Storage.CachedQueries.BapMetadata as CQBapMetaData

-- | Patches Layer 1's on_confirm order: fulfillment-state, breakup titles, BAP/BPP terms, route info, fulfillment.type, vehicle.energy_type.
ondcScheduledRideOnConfirmMessageBuild :: (EsqDBFlow m r, CacheFlow m r, MonadFlow m) => DRB.Booking -> Text -> DBC.BecknConfig -> Spec.ConfirmReqMessage -> m Spec.ConfirmReqMessage
ondcScheduledRideOnConfirmMessageBuild booking bapId bppConfig msg = do
  mbBapMetadata <- CQBapMetaData.findBySubscriberIdAndDomain (Id bapId) Domain.MOBILITY
  let orderWithBreakupTitles = OSRCommon.overrideOrderBreakupTitles msg.confirmReqMessageOrder
  orderWithOverrides <- OSRCommon.applyOnConfirmOrderOverrides booking.isScheduled booking.transactionId mbBapMetadata bppConfig orderWithBreakupTitles
  pure msg {Spec.confirmReqMessageOrder = orderWithOverrides}
