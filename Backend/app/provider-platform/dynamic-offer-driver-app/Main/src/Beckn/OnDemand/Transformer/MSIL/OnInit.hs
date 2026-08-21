-- | MSIL pilot: Layer 2 builder for /on_init. Layer 1 (Beckn.ACL.OnInit.mkOnInitMessageV2,
-- untouched) builds the on_init message exactly as it does for every merchant today.
-- 'msilOnInitMessageBuild' takes that already-built message and, in one pass, adds
-- BPP_TERMS (STATIC_TERMS + OFFLINE_CONTRACT, from beckn_config) to order.tags,
-- ROUTE_INFO (WAYPOINTS + ENCODED_POLYLINE, from the fallback route cached at
-- search time) to every fulfillment's tags, overrides fulfillment.type per the
-- RIDE_OTP->SELF_PICKUP/otherwise->DELIVERY rule, and overrides
-- vehicle.energy_type to a valid ONDC v2.1.0 code (all
-- Beckn.OnDemand.Utils.MSIL.Common).
module Beckn.OnDemand.Transformer.MSIL.OnInit
  ( msilOnInitMessageBuild,
  )
where

import Beckn.OnDemand.Utils.MSIL.Breakup (overrideOrderBreakupTitles)
import qualified Beckn.OnDemand.Utils.MSIL.Common as MSILCommon
import qualified BecknV2.OnDemand.Types as Spec
import qualified Domain.Types.BecknConfig as DBC
import Kernel.Prelude
import Kernel.Utils.Common (CacheFlow, MonadFlow)

-- | Layer 2: takes the already-built on_init message from Layer 1 and, in one
-- pass over the order, remaps quote.breakup[*].title to ONDC's allowed
-- vocabulary, adds BPP_TERMS to order.tags, ROUTE_INFO to every fulfillment's
-- tags, overrides every fulfillment's type code, and overrides every
-- fulfillment's vehicle.energy_type. Every other field is passed through
-- untouched.
msilOnInitMessageBuild :: (CacheFlow m r, MonadFlow m) => Text -> DBC.BecknConfig -> Spec.ConfirmReqMessage -> m Spec.ConfirmReqMessage
msilOnInitMessageBuild transactionId bppConfig msg = do
  let orderWithBreakupTitles = overrideOrderBreakupTitles msg.confirmReqMessageOrder
  orderWithOverrides <- MSILCommon.applyOnInitOrderOverrides transactionId bppConfig orderWithBreakupTitles
  pure msg {Spec.confirmReqMessageOrder = orderWithOverrides}
