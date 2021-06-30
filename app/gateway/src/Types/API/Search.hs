module Types.API.Search
  ( OnSearchReq,
    SearchReq (..),
    OnSearchAPI,
    SearchAPI,
    onSearchAPI,
    searchAPI,
  )
where

import Beckn.Types.Core.Ack
import Beckn.Utils.Servant.SignatureAuth
import Data.Aeson (Value)
import EulerHS.Prelude
import Servant hiding (Context)
import Types.Beckn.API.Callback
import Types.Beckn.Context

data SearchReq = SearchReq
  { context :: Context,
    message :: Value
  }
  deriving (Generic, Show, FromJSON, ToJSON)

type OnSearchReq = CallbackReq Value

type SearchAPI =
  SignatureAuth "Authorization" LookupRegistry
    :> "search"
    :> ReqBody '[JSON] SearchReq
    :> Post '[JSON] AckResponse

searchAPI :: Proxy SearchAPI
searchAPI = Proxy

type OnSearchAPI =
  SignatureAuth "Authorization" LookupRegistry
    :> "on_search"
    :> ReqBody '[JSON] OnSearchReq
    :> Post '[JSON] AckResponse

onSearchAPI :: Proxy OnSearchAPI
onSearchAPI = Proxy
