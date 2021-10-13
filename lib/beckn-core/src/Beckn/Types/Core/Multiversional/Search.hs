module Beckn.Types.Core.Multiversional.Search where

import Beckn.Types.Core.Ack
import Beckn.Types.Core.Multiversional.Callback
import Data.Aeson (Value)
import EulerHS.Prelude
import Servant hiding (Context)

data SearchReq = SearchReq
  { context :: Value,
    message :: Value
  }
  deriving (Generic, Show, FromJSON, ToJSON)

type OnSearchReq = CallbackReq

type SearchAPI =
  "search"
    :> ReqBody '[JSON] SearchReq
    :> Post '[JSON] AckResponse

searchAPI :: Proxy SearchAPI
searchAPI = Proxy

type OnSearchAPI =
  "on_search"
    :> ReqBody '[JSON] OnSearchReq
    :> Post '[JSON] AckResponse

onSearchAPI :: Proxy OnSearchAPI
onSearchAPI = Proxy
