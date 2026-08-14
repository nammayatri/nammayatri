-- | MSIL pilot: Layer 2 patch for /on_confirm's fulfillment state. Layer 1
-- (Beckn.ACL.OnConfirm.bookingStatusCode, buildOnConfirmMessageV2 -- both
-- untouched) returns Enums.NEW for the no-driver-yet static/scheduled path.
-- NEW does not appear anywhere in the ONDC v2.1.0 fulfillmentState vocabulary --
-- it's explicitly documented as "Custom type only used for on-us transaction"
-- (BecknV2.OnDemand.Enums.FulfillmentState) -- while the reference on_confirm
-- example uses RIDE_CONFIRMED for exactly this case (order confirmed, no driver
-- yet). Gated behind scheduledCategorySignalMerchantIds initially rather than a
-- direct Layer 1 fix, because unlike the other MSIL modules this touches a value
-- on a call already live in production for every merchant today -- see doc 25 s8
-- for the promotion plan once this is confirmed safe against real traffic.
--
-- This fulfillment-state fix, the BAP_TERMS/BPP_TERMS order-tag patch
-- (Beckn.OnDemand.Utils.MSIL.Terms.patchOrderTags), the ROUTE_INFO
-- fulfillment-tag patch (Beckn.OnDemand.Utils.MSIL.RouteInfo.patchOrderRouteInfo),
-- the fulfillment.type override (Beckn.OnDemand.Utils.MSIL.FulfillmentType),
-- and the vehicle.energy_type override
-- (Beckn.OnDemand.Utils.MSIL.VehicleEnergyType) are all applied to the
-- on_confirm message in one pass, by 'msilOnConfirmMessageBuild'.
module Beckn.OnDemand.Transformer.MSIL.OnConfirm
  ( msilOnConfirmMessageBuild,
  )
where

import qualified Beckn.OnDemand.Utils.MSIL.FulfillmentType as MSILFulfillmentType
import qualified Beckn.OnDemand.Utils.MSIL.RouteInfo as MSILRouteInfo
import qualified Beckn.OnDemand.Utils.MSIL.Terms as MSILTerms
import qualified Beckn.OnDemand.Utils.MSIL.VehicleEnergyType as MSILVehicleEnergyType
import qualified BecknV2.OnDemand.Enums as Enums
import qualified BecknV2.OnDemand.Types as Spec
import qualified Domain.Types.BecknConfig as DBC
import Kernel.Prelude
import Kernel.Utils.Common (CacheFlow, MonadFlow)

-- | Layer 2: takes the already-built on_confirm message from Layer 1 and, in
-- one pass over the order, (a) rewrites every occurrence of the NEW
-- fulfillment-state code -- both the top-level fulfillment state and the
-- cancellation_terms declarations that echo it -- to RIDE_CONFIRMED, (b)
-- adds BAP_TERMS (echoing the BAP's own declared terms, if known) and
-- BPP_TERMS (ours, from becknConfig) to order.tags, (c) adds ROUTE_INFO
-- (WAYPOINTS + ENCODED_POLYLINE, from the fallback route cached at search
-- time under this transactionId) to every fulfillment's tags, (d) overrides
-- every fulfillment's type code, and (e) overrides every fulfillment's
-- vehicle.energy_type. Every other field is passed through untouched.
msilOnConfirmMessageBuild :: (CacheFlow m r, MonadFlow m) => Text -> Maybe BaseUrl -> DBC.BecknConfig -> Spec.ConfirmReqMessage -> m Spec.ConfirmReqMessage
msilOnConfirmMessageBuild transactionId mbBapStaticTermsUrl bppConfig msg = do
  let orderWithTerms = MSILTerms.patchOrderTags True mbBapStaticTermsUrl bppConfig (fixOrder (Spec.confirmReqMessageOrder msg))
      orderWithFulfillmentType = MSILFulfillmentType.patchOrderFulfillmentTypes orderWithTerms
      orderWithEnergyType = MSILVehicleEnergyType.patchOrderVehicleEnergyType orderWithFulfillmentType
  orderWithRouteInfo <- MSILRouteInfo.patchOrderRouteInfo transactionId orderWithEnergyType
  pure msg {Spec.confirmReqMessageOrder = orderWithRouteInfo}
  where
    fixOrder order =
      order
        { Spec.orderFulfillments = map fixFulfillment <$> Spec.orderFulfillments order,
          Spec.orderCancellationTerms = map fixCancellationTerm <$> Spec.orderCancellationTerms order
        }
    fixFulfillment fulfillment = fulfillment {Spec.fulfillmentState = fixState <$> Spec.fulfillmentState fulfillment}
    fixCancellationTerm term = term {Spec.cancellationTermFulfillmentState = fixState <$> Spec.cancellationTermFulfillmentState term}
    fixState fulfillmentState = fulfillmentState {Spec.fulfillmentStateDescriptor = fixDescriptor <$> Spec.fulfillmentStateDescriptor fulfillmentState}
    fixDescriptor descriptor
      | Spec.descriptorCode descriptor == Just (show Enums.NEW) = descriptor {Spec.descriptorCode = Just (show Enums.RIDE_CONFIRMED)}
      | otherwise = descriptor
