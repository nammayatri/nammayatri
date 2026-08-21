-- | MSIL pilot: Layer 2 patch for /on_confirm's fulfillment state. Layer 1
-- (Beckn.ACL.OnConfirm.bookingStatusCode, buildOnConfirmMessageV2 -- both
-- untouched) returns Enums.NEW for the no-driver-yet static/scheduled path,
-- and SharedLogic.CallBAP.buildOnConfirmMessage (the later, driver-assigned
-- push, also untouched) returns Enums.SCHEDULED_RIDE_ASSIGNED for a scheduled
-- ride's assignment. Neither code appears in the ONDC v2.1.0 fulfillmentState
-- vocabulary -- see Beckn.OnDemand.Utils.MSIL.Common.overrideOrderFulfillmentState
-- for the actual overrides and why. Gated behind enableScheduledCategorySignal
-- initially rather than a direct Layer 1 fix, because unlike the other MSIL
-- patches this touches a value on a call already live in production for every
-- merchant today -- see doc 25 s8 for the promotion plan once this is
-- confirmed safe against real traffic.
--
-- This fulfillment-state fix, the BAP_TERMS/BPP_TERMS order-tag patch
-- (Beckn.OnDemand.Utils.MSIL.Common.patchOrderTags), the ROUTE_INFO
-- fulfillment-tag patch (Beckn.OnDemand.Utils.MSIL.Common.patchOrderRouteInfo),
-- the fulfillment.type override (Beckn.OnDemand.Utils.MSIL.Common.patchOrderFulfillmentTypes),
-- and the vehicle.energy_type override
-- (Beckn.OnDemand.Utils.MSIL.Common.patchOrderVehicleEnergyType) are all applied
-- to the on_confirm message in one pass, by 'msilOnConfirmMessageBuild'.
module Beckn.OnDemand.Transformer.MSIL.OnConfirm
  ( msilOnConfirmMessageBuild,
  )
where

import Beckn.OnDemand.Utils.MSIL.Breakup (overrideOrderBreakupTitles)
import qualified Beckn.OnDemand.Utils.MSIL.Common as MSILCommon
import qualified BecknV2.OnDemand.Types as Spec
import qualified Domain.Types.BecknConfig as DBC
import Kernel.Prelude
import Kernel.Utils.Common (CacheFlow, MonadFlow)

-- | Layer 2: takes the already-built on_confirm message from Layer 1 and, in
-- one pass over the order, (a) overrides non-ONDC-compliant fulfillment-state
-- codes -- both the top-level fulfillment state and the cancellation_terms
-- declarations that echo it, (b) adds BAP_TERMS (echoing the BAP's own
-- declared terms, if known) and BPP_TERMS (ours, from becknConfig) to
-- order.tags, (c) adds ROUTE_INFO (WAYPOINTS + ENCODED_POLYLINE, from the
-- fallback route cached at search time under this transactionId) to every
-- fulfillment's tags, (d) overrides every fulfillment's type code, and (e)
-- overrides every fulfillment's vehicle.energy_type. Every other field is
-- passed through untouched. See MSILCommon.applyOnConfirmOrderOverrides.
msilOnConfirmMessageBuild :: (CacheFlow m r, MonadFlow m) => Bool -> Text -> Maybe BaseUrl -> DBC.BecknConfig -> Spec.ConfirmReqMessage -> m Spec.ConfirmReqMessage
msilOnConfirmMessageBuild isScheduled transactionId mbBapStaticTermsUrl bppConfig msg = do
  let orderWithBreakupTitles = overrideOrderBreakupTitles msg.confirmReqMessageOrder
  orderWithOverrides <- MSILCommon.applyOnConfirmOrderOverrides isScheduled transactionId mbBapStaticTermsUrl bppConfig orderWithBreakupTitles
  pure msg {Spec.confirmReqMessageOrder = orderWithOverrides}
