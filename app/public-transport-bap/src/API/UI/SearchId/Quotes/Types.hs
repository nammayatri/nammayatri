module API.UI.SearchId.Quotes.Types where

import Beckn.Types.Id
import Domain.Endpoints.UI.Quotes as DQuotes
import qualified Domain.Types.Search as DSearch
import Servant
import Tools.Auth

type API =
  Capture "searchId" (Id DSearch.Search)
    :> TokenAuth
    :> "quotes"
    :> Get '[JSON] DQuotes.GetQuotesRes
