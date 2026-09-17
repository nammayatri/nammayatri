-- | MSIL pilot: Layer 1's mkStopsOUS (Beckn.OnDemand.Utils.Common) hardcodes
-- the START stop's OTP authorization.status as "UNCLAIMED" on every outgoing
-- order payload, regardless of whether the driver already validated that OTP
-- and started the ride. ONDC Workbench expects it to flip to "CLAIMED" once
-- the ride has actually started -- i.e. for RIDE_STARTED and every event
-- built after it (ride completed, status polls, ...) -- and NACKs otherwise.
--
-- This patches the START stop's authorization.status to "CLAIMED" for MSIL
-- once the ride has started; it is a no-op (leaves Layer 1's "UNCLAIMED"
-- alone) for anything built before the ride starts, e.g. the RIDE_ASSIGNED
-- on_confirm/on_update.
module Beckn.OnDemand.Utils.MSIL.StopAuthorization
  ( overrideOrderStopAuthorizationStatus,
  )
where

import qualified BecknV2.OnDemand.Types as Spec
import Kernel.Prelude

overrideOrderStopAuthorizationStatus :: Bool -> Spec.Order -> Spec.Order
overrideOrderStopAuthorizationStatus isRideStarted order
  | not isRideStarted = order
  | otherwise = order {Spec.orderFulfillments = map patchFulfillment <$> order.orderFulfillments}
  where
    patchFulfillment fulfillment = fulfillment {Spec.fulfillmentStops = map patchStop <$> fulfillment.fulfillmentStops}
    patchStop stop
      | stop.stopType == Just "START" = stop {Spec.stopAuthorization = claim <$> stop.stopAuthorization}
      | otherwise = stop
    claim auth = auth {Spec.authorizationStatus = Just "CLAIMED"}
