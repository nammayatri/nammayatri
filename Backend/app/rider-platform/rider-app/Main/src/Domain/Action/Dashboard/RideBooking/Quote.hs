module Domain.Action.Dashboard.RideBooking.Quote (getQuoteResult) where

import qualified API.UI.Quote
import qualified "this" Domain.Action.UI.Quote as DQuote
import qualified Domain.Types.Merchant
import qualified "this" Domain.Types.Person
import qualified "this" Domain.Types.SearchRequest
import qualified Environment
import Kernel.Prelude
import qualified Kernel.Types.Beckn.Context
import Kernel.Types.Common (mkPrice, mkPriceAPIEntity)
import qualified Kernel.Types.Id
import Kernel.Utils.Common (logInfo)
import SharedLogic.Merchant (findMerchantByShortId)
import qualified Storage.Queries.Quote as QQuote

-- | Dashboard-only quote result. Reuses the shared UI quote flow, then enriches
-- each quote's fare breakup with a "COMMISSION" line sourced from the persisted
-- Quote.commissionCharges (populated from the BPP on_search callback). Commission
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
  quotes <- QQuote.findAllBySRId searchId
  let commissionBreakupByQuoteId =
        mapMaybe
          (\q -> (\cc -> (q.id, mkCommissionBreakup q.estimatedFare.currency cc)) <$> q.commissionCharges)
          quotes
  logInfo $ "getQuoteResult: searchId=" <> searchId.getId <> " has " <> show (length commissionBreakupByQuoteId) <> " of " <> show (length quotes) <> " quotes with commissionCharges set; injecting COMMISSION breakup for dashboard response"
  pure res {DQuote.quotes = map (addCommissionBreakup commissionBreakupByQuoteId) res.quotes}
  where
    mkCommissionBreakup currency commissionCharges =
      DQuote.QuoteBreakupAPIEntity
        { title = "COMMISSION",
          priceWithCurrency = mkPriceAPIEntity (mkPrice (Just currency) commissionCharges)
        }
    addCommissionBreakup commissionBreakups = \case
      DQuote.OnDemandCab q -> DQuote.OnDemandCab (addBreakup commissionBreakups q)
      DQuote.OnRentalCab q -> DQuote.OnRentalCab (addBreakup commissionBreakups q)
      DQuote.OnMeterRide q -> DQuote.OnMeterRide (addBreakup commissionBreakups q)
      other -> other
    addBreakup commissionBreakups q =
      case lookup q.id commissionBreakups of
        Just breakup -> q {DQuote.quoteFareBreakup = q.quoteFareBreakup <> [breakup]}
        Nothing -> q
