module API.UI.SearchId.Quotes.Types where

import Domain.Action.UI.Quotes as DQuotes
import qualified Domain.Types.Search as DSearch
import Kernel.Types.Id
import Servant
import Tools.Auth

type API =
  Capture "searchId" (Id DSearch.Search)
    :> TokenAuth
    :> "quotes"
    :> Get '[JSON] DQuotes.GetQuotesRes
