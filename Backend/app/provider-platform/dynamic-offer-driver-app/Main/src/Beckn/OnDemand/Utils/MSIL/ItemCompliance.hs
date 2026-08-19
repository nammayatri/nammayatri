-- | MSIL pilot: order.items[*] compliance fix for ONDC v2.1.0's TRV10 schema,
-- for order-carrying messages built from Layer 1 paths that never route
-- through the on_search catalog builder -- concretely, the dynamic-offer
-- (estimate-based, "on-demand") /select flow's on_select
-- (SharedLogic.CallBAP.sendDriverOffer -> Beckn.ACL.OnSelect.mkOnSelectMessageV2),
-- which has no MSIL override applied at all today, unlike the Quote-based
-- (scheduled) /select flow's on_select and every downstream on_confirm/
-- on_update/on_status message.
--
-- Mirrors Beckn.OnDemand.Transformer.MSIL.OnSearch's msilPatchCatalogCompliance,
-- which fixes the identical two problems one layer earlier for catalog.providers[*].items
-- (a structurally different field, Provider.providerItems, from order.items --
-- hence the separate top-level function here even though the per-item Item
-- type and the fix itself are identical): item.descriptor.code restricted to
-- ["RIDE", "RENTAL"] (Layer 1 emits the raw vehicle category/variant, e.g.
-- "AUTO_RICKSHAW"), and item.tags[*].descriptor.code restricted to a fixed
-- allow-list (Layer 1's FARE_POLICY/FEATURE_LIST tag groups pass the check
-- already; any others get dropped rather than sent non-compliant).
module Beckn.OnDemand.Utils.MSIL.ItemCompliance
  ( overrideOrderItemCompliance,
  )
where

import qualified BecknV2.OnDemand.Types as Spec
import Data.Text (isInfixOf)
import EulerHS.Prelude

overrideOrderItemCompliance :: Spec.Order -> Spec.Order
overrideOrderItemCompliance order = order {Spec.orderItems = map fixItem <$> Spec.orderItems order}
  where
    isRental item = maybe False (any ("RENTAL" `isInfixOf`)) (Spec.itemCategoryIds item)
    fixItem item =
      item
        { Spec.itemDescriptor = fixItemDescriptor <$> Spec.itemDescriptor item,
          Spec.itemTags = filter isAllowedTagGroup <$> Spec.itemTags item
        }
      where
        fixItemDescriptor descriptor = descriptor {Spec.descriptorCode = Just (if isRental item then "RENTAL" else "RIDE")}

    allowedTagGroupCodes :: [Text]
    allowedTagGroupCodes =
      [ "DISABILITY_VIS",
        "DISABILITY_HEA",
        "DISABILITY_MOB",
        "DISABILITY_LEP",
        "DISABILITY_SPE",
        "DISABILITY_INTEL",
        "MENTAL",
        "DISABILITY_BLOOD",
        "DISABILITY_DWARFISM",
        "DISABILITY_ACID_ATTACK_SURVIVOR",
        "DISABILITY_MULTIPLE_DIS",
        "FARE_POLICY",
        "INFO",
        "FEATURE_LIST"
      ]
    isAllowedTagGroup tagGroup =
      maybe False (`elem` allowedTagGroupCodes) (Spec.tagGroupDescriptor tagGroup >>= Spec.descriptorCode)
