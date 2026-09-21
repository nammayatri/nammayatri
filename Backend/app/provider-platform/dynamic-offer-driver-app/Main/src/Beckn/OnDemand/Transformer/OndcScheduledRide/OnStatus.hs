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
import qualified Domain.Types.Beckn.Status as DStatus
import Kernel.Prelude
import qualified Kernel.Types.Beckn.Context as Context
import Kernel.Utils.Common (CacheFlow, EsqDBFlow)

-- | Re-wraps an on_update message as on_status; overrides fulfillment.type and vehicle.energy_type, and echoes back the selected add-ons.
ondcScheduledRideOnStatusMessageBuild :: (EsqDBFlow m r, CacheFlow m r) => Bool -> Text -> [DAddOnConfig.AddOnData] -> Spec.OnUpdateReq -> m Spec.OnStatusReq
ondcScheduledRideOnStatusMessageBuild isScheduled quoteId addOnData req = do
  onStatusReqMessage <- traverse patchOrder req.onUpdateReqMessage
  pure
    Spec.OnStatusReq
      { onStatusReqContext = req.onUpdateReqContext {Spec.contextAction = A.decode (A.encode Context.ON_STATUS)},
        onStatusReqError = req.onUpdateReqError,
        onStatusReqMessage
      }
  where
    patchOrder msg = do
      patchedOrder <- OSRCommon.applyOndcScheduledRideAssignedOrderOverrides isScheduled quoteId False addOnData msg.confirmReqMessageOrder
      pure msg {Spec.confirmReqMessageOrder = patchedOrder}

-- | Patches an already-built /status order with the same ONDC overrides the on_confirm/on_update
-- pushes apply, so a BAP diffing the pulled /status against the pushes sees identical fields.
ondcScheduledRideStatusReqBuild :: (EsqDBFlow m r, CacheFlow m r) => DStatus.DStatusRes -> Spec.OnStatusReq -> m Spec.OnStatusReq
ondcScheduledRideStatusReqBuild dStatusRes onStatusReq = do
  onStatusReqMessage <- traverse patchOrder onStatusReq.onStatusReqMessage
  pure onStatusReq {Spec.onStatusReqMessage = onStatusReqMessage}
  where
    booking = dStatusRes.booking
    isRideStarted = case dStatusRes.info of
      DStatus.RideStartedReq _ -> True
      DStatus.RideCompletedReq _ -> True
      _ -> False
    patchOrder msg = do
      patchedOrder <- OSRCommon.applyOndcScheduledRideAssignedOrderOverrides booking.isScheduled booking.quoteId isRideStarted booking.addOnData msg.confirmReqMessageOrder
      pure msg {Spec.confirmReqMessageOrder = patchedOrder}
