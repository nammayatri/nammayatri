module Types.API.Gateway.Search where

import Beckn.Types.Core.Ack
import Beckn.Utils.Servant.JSONBS
import Beckn.Utils.SignatureAuth
import EulerHS.Prelude
import Servant

type SearchAPI =
  "search"
    :> Header "Authorization" SignaturePayload
    :> ReqBody '[JSONBS] ByteString
    :> Post '[JSON] AckResponse

searchAPI :: Proxy SearchAPI
searchAPI = Proxy

type OnSearchAPI =
  "on_search"
    :> Header "Authorization" SignaturePayload
    :> ReqBody '[JSONBS] ByteString
    :> Post '[JSON] AckResponse

onSearchAPI :: Proxy OnSearchAPI
onSearchAPI = Proxy
