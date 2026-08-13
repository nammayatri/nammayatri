-- | MSIL pilot: re-wrap an already-built /on_update message (e.g. the driver
-- arrival push, Beckn.ACL.OnUpdate.buildOnUpdateMessageV2, untouched) as an
-- /on_status message instead. Layer 1 (SharedLogic.CallBAP.sendDriverArrivalUpdateToBAP)
-- keeps building the same message content and keeps pushing everyone else via
-- callOnUpdateV2/on_update exactly as today; this module and its call site are
-- the only things that change, and only for pilot merchants.
--
-- Why gated: today's sendDriverArrivalUpdateToBAP pushes RIDE_ARRIVED_PICKUP via
-- on_update, but the ONDC v2.1.0 reference flow categorizes this event under
-- on_status. Any BAP already integrated against us is, by construction,
-- listening for this event on the on_update endpoint -- silently switching the
-- endpoint for everyone would risk existing integrations simply not seeing
-- arrival events anymore. See doc 25 s11.
--
-- OnUpdateReq and OnStatusReq share the exact same shape (a Context plus a
-- Maybe ConfirmReqMessage) -- only the context's action differs -- so this is a
-- pure relabeling, not a rebuild: the message content itself is untouched.
module Beckn.OnDemand.Transformer.MSIL.OnStatus
  ( toOnStatusReq,
  )
where

import qualified BecknV2.OnDemand.Types as Spec
import qualified Data.Aeson as A
import EulerHS.Prelude ()
import qualified Kernel.Types.Beckn.Context as Context

toOnStatusReq :: Spec.OnUpdateReq -> Spec.OnStatusReq
toOnStatusReq req =
  Spec.OnStatusReq
    { onStatusReqContext = req.onUpdateReqContext {Spec.contextAction = A.decode (A.encode Context.ON_STATUS)},
      onStatusReqError = req.onUpdateReqError,
      onStatusReqMessage = req.onUpdateReqMessage
    }
