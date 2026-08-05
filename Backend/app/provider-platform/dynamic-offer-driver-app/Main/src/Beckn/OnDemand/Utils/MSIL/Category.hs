-- | MSIL pilot: common override for the wire category id/descriptor code
-- (BecknV2.OnDemand.Utils.Common.tripCategoryToCategoryCode, untouched --
-- Layer 1's own mapping is left exactly as it is for every other merchant),
-- shared across every MSIL transformer that emits a scheduled-trip category
-- id -- on_search's provider-level categories and item.category_ids
-- (Beckn.OnDemand.Transformer.MSIL.OnSearch) and on_select's item.category_ids
-- (Beckn.OnDemand.Transformer.MSIL.OnSelect) must all agree on the same id,
-- or a BAP correlating item.category_ids against the declared
-- catalog.providers[*].categories[*].id will find no match.
module Beckn.OnDemand.Utils.MSIL.Category
  ( scheduledCategoryCode,
    overrideOrderCategoryIds,
  )
where

import qualified BecknV2.OnDemand.Types as Spec
import EulerHS.Prelude

-- | ON_DEMAND_TRIP/ON_DEMAND_RENTAL -> SCHEDULED_TRIP/SCHEDULED_RENTAL; anything else
-- (INTERCITY_TRIP, ON_DEMAND_EASY_BOOKING, ...) passes through unchanged -- the ONDC
-- v2.1.0 schedule_trip spec only defines scheduled variants for trip and rental.
scheduledCategoryCode :: Text -> Text
scheduledCategoryCode = \case
  "ON_DEMAND_TRIP" -> "SCHEDULED_TRIP"
  "ON_DEMAND_RENTAL" -> "SCHEDULED_RENTAL"
  other -> other

-- | Same rewrite as scheduledCategoryCode, applied to every item's
-- category_ids on an order -- for on_confirm/on_update's Booking-based item
-- builders (Beckn.OnDemand.Utils.Common.tfItems,
-- Beckn.OnDemand.Utils.OnUpdate.tfItems), which unlike on_search/on_select
-- have no per-item isScheduled to check, only the booking's own. Explicitly
-- gated by the caller's isScheduled (not unconditional, unlike
-- overrideOrderFulfillmentState) -- MSIL pilot merchants also serve plain,
-- non-scheduled bookings, whose category_ids (ON_DEMAND_TRIP/ON_DEMAND_RENTAL)
-- are already correct and must not be touched.
overrideOrderCategoryIds :: Bool -> Spec.Order -> Spec.Order
overrideOrderCategoryIds isScheduled order
  | not isScheduled = order
  | otherwise = order {Spec.orderItems = map fixItem <$> Spec.orderItems order}
  where
    fixItem item = item {Spec.itemCategoryIds = map scheduledCategoryCode <$> Spec.itemCategoryIds item}
