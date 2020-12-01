module Types.API.Search
  ( OnSearchReq,
    SearchReq (..),
    OnSearchAPI,
    SearchAPI,
    onSearchAPI,
    searchAPI,
  )
where

import Beckn.Types.Core.API.Auth
import Beckn.Types.Core.API.Callback
import Beckn.Types.Core.Ack (AckResponse (..))
import Beckn.Types.Core.Context
import Data.Aeson (Value)
import EulerHS.Prelude
import Servant hiding (Context)

data SearchReq = SearchReq
  { context :: Context,
    message :: Value
  }
  deriving (Generic, Show, FromJSON, ToJSON)

type OnSearchReq = CallbackReq Value

type SearchAPI apiKey =
  BecknAuth
    "Authorization"
    apiKey
    ( "search"
        :> ReqBody '[JSON] SearchReq
        :> Post '[JSON] AckResponse
    )

searchAPI :: Proxy (SearchAPI apiKey)
searchAPI = Proxy

type OnSearchAPI apiKey =
  BecknAuth
    "Authorization"
    apiKey
    ( "on_search"
        :> ReqBody '[JSON] OnSearchReq
        :> Post '[JSON] AckResponse
    )

onSearchAPI :: Proxy (OnSearchAPI apiKey)
onSearchAPI = Proxy
