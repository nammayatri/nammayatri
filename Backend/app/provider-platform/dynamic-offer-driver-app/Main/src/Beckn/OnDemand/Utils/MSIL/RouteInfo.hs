-- | MSIL pilot: common helper for the ROUTE_INFO tag group (WAYPOINTS,
-- ENCODED_POLYLINE), shared across every MSIL transformer that sends it to
-- the BAP (OnSelect, OnInit, OnConfirm, ...).
--
-- ONDC used to have the BAP send ROUTE_INFO (WAYPOINTS) to the BPP at
-- /search, which Beckn.OnDemand.Utils.Search.buildRoutePoints still parses.
-- Per the updated spec, the BPP now computes this itself and sends it back to
-- the BAP instead. Rather than compute a fresh route, this reuses the same
-- fallback route -- already computed once, at search time, whenever the BAP
-- didn't supply routePoints/routeDistance/routeDuration itself
-- (Domain.Action.Beckn.Search.getRouteServiceability) -- which is cached in
-- Redis under the search's transactionId (SharedLogic.Ride.searchRequestKey)
-- for the lifetime of the booking. WAYPOINTS is JSON-encoded (mirroring the
-- format Utils.Search.buildRoutePoints expects when parsing it off the BAP);
-- ENCODED_POLYLINE is the Google-polyline encoding of the same points
-- (Kernel.External.Maps.Google.PolyLinePoints.encode).
--
-- Not sent on on_search -- only on the later, order-bearing responses
-- (on_select/on_init/on_confirm/...), under order.fulfillments[].tags.
module Beckn.OnDemand.Utils.MSIL.RouteInfo
  ( patchOrderRouteInfo,
  )
where

import qualified BecknV2.OnDemand.Tags as Tag
import qualified BecknV2.OnDemand.Types as Spec
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BSL
import qualified Domain.Types.RideRoute as RI
import qualified Kernel.External.Maps.Google.PolyLinePoints as PolyLine
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Utils.Common (CacheFlow, MonadFlow)
import SharedLogic.Ride (searchRequestKey)

-- | Fetch the fallback route (computed and cached at search time, keyed by
-- transactionId) and build the ROUTE_INFO tag group from it. Nothing if no
-- route was ever cached for this transaction, or it has no points.
getRouteInfoTagGroup :: (CacheFlow m r, MonadFlow m) => Text -> m (Maybe Spec.TagGroup)
getRouteInfoTagGroup transactionId = do
  mbRouteInfo :: Maybe RI.RouteInfo <- Redis.runInMultiCloudRedisMaybeResult $ Redis.withMasterRedis $ Redis.get (searchRequestKey transactionId)
  pure $ mbRouteInfo >>= (.points) >>= mkTagGroup
  where
    mkTagGroup [] = Nothing
    mkTagGroup points =
      Just $
        Tag.getFullTagGroup
          Tag.ROUTE_INFO
          [ Tag.mkTag Tag.WAYPOINTS (Just . decodeUtf8 . BSL.toStrict $ Aeson.encode points),
            Tag.mkTag Tag.ENCODED_POLYLINE (Just $ PolyLine.encode points)
          ]

addTagGroup :: Maybe Spec.TagGroup -> Maybe [Spec.TagGroup] -> Maybe [Spec.TagGroup]
addTagGroup Nothing existingGroups = existingGroups
addTagGroup (Just newGroup) existingGroups = Just (fromMaybe [] existingGroups <> [newGroup])

-- | The single patch operation for an order: adds the ROUTE_INFO tag group
-- (if a fallback route was cached for this transactionId) to every one of
-- the order's fulfillments' tags, additive alongside whatever Layer 1
-- already put there.
patchOrderRouteInfo :: (CacheFlow m r, MonadFlow m) => Text -> Spec.Order -> m Spec.Order
patchOrderRouteInfo transactionId order = do
  mbRouteInfoTagGroup <- getRouteInfoTagGroup transactionId
  pure $ order {Spec.orderFulfillments = map (patchFulfillment mbRouteInfoTagGroup) <$> order.orderFulfillments}
  where
    patchFulfillment mbRouteInfoTagGroup fulfillment =
      fulfillment {Spec.fulfillmentTags = addTagGroup mbRouteInfoTagGroup fulfillment.fulfillmentTags}
