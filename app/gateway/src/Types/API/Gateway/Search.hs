module Types.API.Gateway.Search where

import Beckn.Types.Core.Ack (AckResponse (..))
import Beckn.Utils.Servant.HeaderAuth
import Beckn.Utils.SignatureAuth
import EulerHS.Prelude
import Servant
import Types.API.Search (OnSearchReq, SearchReq)
import Utils.Auth

type SearchAPI =
  ( "search"
      :> Header "Authorization" SignaturePayload
      :> ReqBody '[JSON] SearchReq
      :> Post '[JSON] AckResponse
  )
    :<|> ( "search"
             :> APIKeyAuth VerifyAPIKey
             :> ReqBody '[JSON] SearchReq
             :> Post '[JSON] AckResponse
         )

searchAPI :: Proxy SearchAPI
searchAPI = Proxy

type OnSearchAPI =
  ( "on_search"
      :> Header "Authorization" SignaturePayload
      :> ReqBody '[JSON] OnSearchReq
      :> Post '[JSON] AckResponse
  )
    :<|> ( "on_search"
             :> APIKeyAuth VerifyAPIKey
             :> ReqBody '[JSON] OnSearchReq
             :> Post '[JSON] AckResponse
         )

onSearchAPI :: Proxy OnSearchAPI
onSearchAPI = Proxy
