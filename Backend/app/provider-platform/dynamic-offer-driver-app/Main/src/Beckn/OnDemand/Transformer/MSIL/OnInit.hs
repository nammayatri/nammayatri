-- | MSIL pilot: Layer 2 builder for /on_init. Layer 1 (Beckn.ACL.OnInit.mkOnInitMessageV2,
-- untouched) builds the on_init message exactly as it does for every merchant today.
-- 'msilOnInitMessageBuild' takes that already-built message and, in one pass, adds
-- BPP_TERMS (STATIC_TERMS + OFFLINE_CONTRACT, from beckn_config) to order.tags
-- (Beckn.OnDemand.Utils.MSIL.Terms.patchOrderTags), ROUTE_INFO (WAYPOINTS +
-- ENCODED_POLYLINE, from the fallback route cached at search time) to every
-- fulfillment's tags (Beckn.OnDemand.Utils.MSIL.RouteInfo.patchOrderRouteInfo),
-- overrides fulfillment.type per the RIDE_OTP->SELF_PICKUP/otherwise->DELIVERY
-- rule (Beckn.OnDemand.Utils.MSIL.FulfillmentType), and overrides
-- vehicle.energy_type to a valid ONDC v2.1.0 code
-- (Beckn.OnDemand.Utils.MSIL.VehicleEnergyType).
module Beckn.OnDemand.Transformer.MSIL.OnInit
  ( msilOnInitMessageBuild,
  )
where

import qualified Beckn.OnDemand.Utils.MSIL.FulfillmentType as MSILFulfillmentType
import qualified Beckn.OnDemand.Utils.MSIL.RouteInfo as MSILRouteInfo
import qualified Beckn.OnDemand.Utils.MSIL.Terms as MSILTerms
import qualified Beckn.OnDemand.Utils.MSIL.VehicleEnergyType as MSILVehicleEnergyType
import qualified BecknV2.OnDemand.Types as Spec
import qualified Domain.Types.BecknConfig as DBC
import Kernel.Prelude
import Kernel.Utils.Common (CacheFlow, MonadFlow)

-- | Layer 2: takes the already-built on_init message from Layer 1 and, in one
-- pass over the order, adds BPP_TERMS to order.tags, ROUTE_INFO to every
-- fulfillment's tags, overrides every fulfillment's type code, and overrides
-- every fulfillment's vehicle.energy_type. Every other field is passed
-- through untouched.
msilOnInitMessageBuild :: (CacheFlow m r, MonadFlow m) => Text -> DBC.BecknConfig -> Spec.ConfirmReqMessage -> m Spec.ConfirmReqMessage
msilOnInitMessageBuild transactionId bppConfig msg = do
  let orderWithTerms = MSILTerms.patchOrderTags False Nothing bppConfig msg.confirmReqMessageOrder
      orderWithFulfillmentType = MSILFulfillmentType.patchOrderFulfillmentTypes orderWithTerms
      orderWithEnergyType = MSILVehicleEnergyType.patchOrderVehicleEnergyType orderWithFulfillmentType
  orderWithRouteInfo <- MSILRouteInfo.patchOrderRouteInfo transactionId orderWithEnergyType
  pure msg {Spec.confirmReqMessageOrder = orderWithRouteInfo}
