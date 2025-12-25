module Domain.Action.Dashboard.RideBooking.Quote (getQuoteResult) where

import qualified API.UI.Quote
import qualified "this" Domain.Action.UI.Quote
import qualified Domain.Types.Merchant
import qualified "this" Domain.Types.Person
import qualified "this" Domain.Types.SearchRequest
import qualified Environment
import Kernel.Prelude
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import SharedLogic.Merchant (findMerchantByShortId)

getQuoteResult ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  Kernel.Types.Id.Id Domain.Types.SearchRequest.SearchRequest ->
  Kernel.Types.Id.Id Domain.Types.Person.Person ->
  Environment.Flow Domain.Action.UI.Quote.GetQuotesRes
getQuoteResult merchantShortId _opCity searchId personId = do
  m <- findMerchantByShortId merchantShortId
  API.UI.Quote.getQuotes' searchId (personId, m.id) Nothing
