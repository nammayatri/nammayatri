module Domain.Action.Dashboard.RideBooking.Quote (getQuoteResult) where

import qualified API.UI.Quote
import qualified "this" Domain.Action.UI.Estimate as UEstimate
import qualified "this" Domain.Action.UI.Quote as DQuote
import qualified Domain.Types.Merchant
import qualified "this" Domain.Types.Person
import qualified "this" Domain.Types.SearchRequest
import qualified Environment
import Kernel.Prelude
import qualified Kernel.Types.Beckn.Context
import Kernel.Types.Common (mkPrice, mkPriceAPIEntity)
import qualified Kernel.Types.Id
import SharedLogic.Merchant (findMerchantByShortId)
import qualified Storage.Queries.Estimate as QEstimate

-- | Dashboard-only quote result. Reuses the shared UI quote flow, then enriches
-- each estimate's fare breakup with a "COMMISSION" line sourced from the persisted
-- Estimate.commissionCharges (populated from the BPP on_search callback). Commission
-- is provider-side data, so this enrichment lives only here and never affects the
-- customer app quote response.
getQuoteResult ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Kernel.Types.Id.Id Domain.Types.SearchRequest.SearchRequest ->
  Kernel.Types.Id.Id Domain.Types.Person.Person ->
  Environment.Flow DQuote.GetQuotesRes
getQuoteResult merchantShortId _opCity searchId personId = do
  m <- findMerchantByShortId merchantShortId
  res <- API.UI.Quote.getQuotes' searchId (personId, m.id) Nothing
  estimates <- QEstimate.findAllBySRId searchId
  let commissionBreakupByEstimateId =
        mapMaybe
          (\e -> (\cc -> (e.id, mkCommissionBreakup e.estimatedFare.currency cc)) <$> e.commissionCharges)
          estimates
  pure res {DQuote.estimates = map (addCommissionBreakup commissionBreakupByEstimateId) res.estimates}
  where
    mkCommissionBreakup currency commissionCharges =
      let commissionPrice = mkPrice (Just currency) commissionCharges
       in UEstimate.EstimateBreakupAPIEntity
            { title = "COMMISSION",
              price = commissionPrice.amountInt,
              priceWithCurrency = mkPriceAPIEntity commissionPrice
            }
    addCommissionBreakup commissionBreakups est =
      case lookup est.id commissionBreakups of
        Just breakup -> est {UEstimate.estimateFareBreakup = est.estimateFareBreakup <> [breakup]}
        Nothing -> est
