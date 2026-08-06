-- | MSIL-only pilot: Layer 2 of the search-parsing pipeline. Layer 1
-- (Beckn.OnDemand.Transformer.Search.buildSearchReqRaw) always sets
-- DSearchReq.isExplicitlyScheduled to Nothing and is otherwise unaware this module
-- exists. This module runs only for merchants on the
-- AppEnv.scheduledCategorySignalMerchantIds allowlist (dispatched from
-- API.Beckn.Search.search) and patches that one field, from the incoming ONDC v2.1.0
-- category descriptor code -- independent of the pickupTime-vs-now/buffer heuristic
-- Domain.Action.Beckn.Search.getPossibleTripOption uses for every other merchant.
module Beckn.OnDemand.Transformer.MSIL.Search
  ( msilParser,
    isScheduledCategoryCode,
  )
where

import qualified Beckn.OnDemand.Utils.Search as Utils
import qualified BecknV2.OnDemand.Types as Spec
import qualified Domain.Action.Beckn.Search as DSearch
import EulerHS.Prelude

-- | True only when the incoming search's own category explicitly said
-- SCHEDULED_TRIP/SCHEDULED_RENTAL. Deliberately not the same question
-- Utils.mapCategoryCodeToRiderPreferred answers (that picks OneWay vs Rental vs ...,
-- and is left untouched -- this is a companion, not a replacement).
isScheduledCategoryCode :: Maybe Text -> Bool
isScheduledCategoryCode = \case
  Just "SCHEDULED_TRIP" -> True
  Just "SCHEDULED_RENTAL" -> True
  _ -> False

-- | Layer 2: takes the DSearchReq Layer 1 already built (isExplicitlyScheduled = Nothing)
-- plus the original wire message, and returns it with isExplicitlyScheduled decided.
msilParser :: Spec.SearchReqMessage -> DSearch.DSearchReq -> DSearch.DSearchReq
msilParser req dSearchReq =
  dSearchReq {DSearch.isExplicitlyScheduled = Just (isScheduledCategoryCode (Utils.getCategoryCode req))}
