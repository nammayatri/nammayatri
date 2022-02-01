module API.UI.SearchId.Quotes.Types where

import Beckn.Prelude
import Beckn.Types.Id
import qualified Domain.Quote as DQuote
import qualified Domain.Search as DSearch
import Servant
import Tools.Auth

type API =
  Capture "searchId" (Id DSearch.Search)
    :> TokenAuth
    :> "quotes"
    :> Get '[JSON] GetQuotesRes

newtype GetQuotesRes = GetQuotesRes
  { quotes :: [DQuote.QuoteAPIEntity]
  }
  deriving (Generic, ToJSON, ToSchema)