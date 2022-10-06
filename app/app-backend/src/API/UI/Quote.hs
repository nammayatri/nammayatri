module API.UI.Quote
  ( DQuote.GetQuotesRes (..),
    DQuote.OfferRes (..),
    API,
    handler,
  )
where

import Beckn.Types.Id
import Beckn.Utils.Common
import qualified Domain.Action.UI.Quote as DQuote
import qualified Domain.Types.Person as Person
import qualified Domain.Types.SearchRequest as SSR
import Environment
import EulerHS.Prelude hiding (id)
import Servant
import Tools.Auth

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
