-- Re-wraps an already-built on_update message as on_status and patches its order for ONDC compliance, since ONDC v2.1.0 categorizes driver-arrival under on_status for pilot merchants only.
module Beckn.OnDemand.Transformer.OndcScheduledRide.OnStatus
  ( ondcScheduledRideOnStatusMessageBuild,
    ondcScheduledRideStatusReqBuild,
  )
where

import qualified Beckn.OnDemand.Utils.OndcScheduledRide.Common as OSRCommon
import qualified BecknV2.OnDemand.Types as Spec
import qualified Data.Aeson as A
import qualified Domain.Types.AddOnConfig as DAddOnConfig
import Kernel.Prelude
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Utils.Common (CacheFlow, EsqDBFlow)

-- | Re-wraps an on_update message as on_status; overrides fulfillment.type and vehicle.energy_type, and echoes back the selected add-ons.
ondcScheduledRideOnStatusMessageBuild :: (EsqDBFlow m r, CacheFlow m r) => [DAddOnConfig.AddOnData] -> Spec.OnUpdateReq -> m Spec.OnStatusReq
ondcScheduledRideOnStatusMessageBuild addOnData req = do
  onStatusReqMessage <- traverse patchOrder req.onUpdateReqMessage
  pure
    Spec.OnStatusReq
      { onStatusReqContext = req.onUpdateReqContext {Spec.contextAction = A.decode (A.encode Context.ON_STATUS)},
        onStatusReqError = req.onUpdateReqError,
        onStatusReqMessage
      }
  where
    patchOrder msg = do
      orderWithAddOns <- OSRCommon.patchOrderAddOns addOnData (OSRCommon.applyOnStatusOrderOverrides msg.confirmReqMessageOrder)
      pure msg {Spec.confirmReqMessageOrder = orderWithAddOns}

-- | Patches an already-built /status order for ONDC compliance: tags, breakup titles, fulfillment.type, category ids.
ondcScheduledRideStatusReqBuild :: Bool -> Spec.OnStatusReq -> Spec.OnStatusReq
ondcScheduledRideStatusReqBuild isScheduled onStatusReq =
  onStatusReq {Spec.onStatusReqMessage = patchOrder <$> onStatusReq.onStatusReqMessage}
  where
    patchOrder msg = msg {Spec.confirmReqMessageOrder = fixOrder msg.confirmReqMessageOrder}
    fixOrder =
      OSRCommon.dropNonConformingOrderTags
        . OSRCommon.patchOrderFulfillmentTypes
        . OSRCommon.overrideOrderCategoryIds isScheduled
        . OSRCommon.overrideOrderBreakupTitles
