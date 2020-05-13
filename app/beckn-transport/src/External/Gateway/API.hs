module External.Gateway.API where

import Beckn.Types.API.Search
import EulerHS.Prelude
import qualified EulerHS.Types as ET
import External.Gateway.Types
import Servant
import Servant.API.ContentTypes
import Servant.Client

type SearchAPI =
  "on_search"
    :> "services"
    :> ReqBody '[JSON] OnSearchReq
    :> Post '[JSON] OnSearchRes

searchAPI :: Proxy SearchAPI
searchAPI = Proxy

onSearch req =
  void $ ET.client searchAPI req
