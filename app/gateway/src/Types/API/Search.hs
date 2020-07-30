module Types.API.Search
  ( OnSearchReq (..),
    SearchReq (..),
    OnSearchAPI,
    SearchAPI,
    onSearchAPI,
    searchAPI,
  )
where

import Beckn.Types.Common (AckResponse (..))
import Beckn.Types.Core.Context
import Beckn.Types.Core.Error
import Beckn.Utils.Servant.HeaderAuth (APIKeyAuth)
import Data.Aeson (Value)
import EulerHS.Prelude
import Servant hiding (Context)
import Utils.Auth (VerifyAPIKey)

data SearchReq = SearchReq
  { context :: Context,
    message :: Value
  }
  deriving (Generic, Show, FromJSON, ToJSON)

data OnSearchReq = OnSearchReq
  { context :: Context,
    message :: Value,
    error :: Maybe Error
  }
  deriving (Generic, Show, FromJSON, ToJSON)

type SearchAPI =
  "search"
    :> APIKeyAuth VerifyAPIKey
    :> ReqBody '[JSON] SearchReq
    :> Post '[JSON] AckResponse

searchAPI :: Proxy SearchAPI
searchAPI = Proxy

type OnSearchAPI =
  "on_search"
    :> APIKeyAuth VerifyAPIKey
    :> ReqBody '[JSON] OnSearchReq
    :> Post '[JSON] AckResponse

onSearchAPI :: Proxy OnSearchAPI
onSearchAPI = Proxy
