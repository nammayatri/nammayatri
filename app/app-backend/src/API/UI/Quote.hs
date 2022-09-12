module API.UI.Quote
  ( DQuote.GetQuotesRes (..),
    DQuote.OfferRes (..),
    API,
    handler,
  )
where

import App.Types
import Beckn.Types.Id
import qualified Domain.Action.UI.Quote as DQuote
import qualified Domain.Types.Person as Person
import qualified Domain.Types.SearchRequest as SSR
import EulerHS.Prelude hiding (id)
import Servant
import Utils.Auth
import Utils.Common

type API =
  "rideSearch"
    :> Capture "searchId" (Id SSR.SearchRequest)
    :> TokenAuth
    :> "results"
    :> Get '[JSON] DQuote.GetQuotesRes

handler :: FlowServer API
handler =
  getQuotes

getQuotes :: Id SSR.SearchRequest -> Id Person.Person -> FlowHandler DQuote.GetQuotesRes
getQuotes searchRequestId _ = withFlowHandlerAPI $ DQuote.getQuotes searchRequestId
