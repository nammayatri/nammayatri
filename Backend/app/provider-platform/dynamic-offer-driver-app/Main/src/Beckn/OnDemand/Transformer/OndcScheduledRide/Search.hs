-- Derives isSchedule from the incoming ONDC category code and verifies+stores the BAP's static terms, since Layer 1 always leaves isSchedule as Nothing and doesn't know about the ONDC pilot.
module Beckn.OnDemand.Transformer.OndcScheduledRide.Search
  ( ondcScheduledRideParser,
    isScheduledCategoryCode,
  )
where

import qualified Beckn.OnDemand.Utils.Search as Utils
import qualified BecknV2.OnDemand.Types as Spec
import qualified Domain.Action.Beckn.Search as DSearch
import EulerHS.Prelude

-- | True only when the incoming search's category explicitly said
-- SCHEDULED_TRIP/SCHEDULED_RENTAL.
isScheduledCategoryCode :: Maybe Text -> Bool
isScheduledCategoryCode = \case
  Just "SCHEDULED_TRIP" -> True
  Just "SCHEDULED_RENTAL" -> True
  _ -> False

-- | Sets isSchedule on Layer 1's DSearchReq.
ondcScheduledRideParser :: Spec.SearchReqMessage -> DSearch.DSearchReq -> DSearch.DSearchReq
ondcScheduledRideParser req dSearchReq =
  dSearchReq {DSearch.isSchedule = Just (isScheduledCategoryCode (Utils.getCategoryCode req))}
