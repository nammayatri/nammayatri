-- | MSIL-only pilot: Layer 2 of the select-parsing pipeline. Layer 1
-- (Beckn.ACL.Select.buildSelectReqV2) always sets DSelectReq.negotiatedFare to
-- Nothing and is otherwise unaware this module exists. This module runs only
-- for merchants on the TransporterConfig.enableScheduledCategorySignal city-level gate
-- (dispatched from API.Beckn.Select.select) and patches that one field, from
-- the incoming ONDC v2.1.0 Pre-Order Bid item.price.value -- independent of
-- the tag/breakup-based customerExtraFee parsing Layer 1 does for every other
-- merchant (see doc 28).
module Beckn.OnDemand.Transformer.MSIL.Select
  ( msilParser,
  )
where

import qualified BecknV2.OnDemand.Types as Spec
import qualified Domain.Action.Beckn.Select as DSelect
import EulerHS.Prelude
import Kernel.Types.Common (HighPrecMoney)
import Kernel.Types.Price (highPrecMoneyFromText)

-- | The BAP's proposed total fare, read from the wire item's own price object
-- (item.price.value) -- not a tag, not a quote breakup line. Only the single
-- order item /select already requires (Beckn.ACL.Select enforces exactly one)
-- is considered.
getNegotiatedFare :: Spec.ConfirmReqMessage -> Maybe HighPrecMoney
getNegotiatedFare message = do
  items <- message.confirmReqMessageOrder.orderItems
  item <- case items of
    [i] -> Just i
    _ -> Nothing
  price <- item.itemPrice
  priceValue <- price.priceValue
  highPrecMoneyFromText priceValue

-- | Layer 2: takes the DSelectReq Layer 1 already built (negotiatedFare =
-- Nothing) plus the original wire message, and returns it with negotiatedFare
-- decided.
msilParser :: Spec.ConfirmReqMessage -> DSelect.DSelectReq -> DSelect.DSelectReq
msilParser message dSelectReq =
  dSelectReq {DSelect.negotiatedFare = getNegotiatedFare message}
