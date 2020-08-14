{-# LANGUAGE DuplicateRecordFields #-}

module Beckn.Types.FMD.API.Search where

import Beckn.Types.API.Callback
import Beckn.Types.Common
import Beckn.Types.Core.Catalog
import Beckn.Types.Core.Context
import Beckn.Types.FMD.Intent
import Beckn.Utils.Servant.HeaderAuth
import Data.Generics.Labels ()
import EulerHS.Prelude
import Servant (JSON, Post, ReqBody, (:>))

type SearchAPI v =
  "search"
    :> APIKeyAuth v
    :> ReqBody '[JSON] SearchReq
    :> Post '[JSON] AckResponse

searchAPI :: Proxy (SearchAPI v)
searchAPI = Proxy

type OnSearchAPI v =
  "on_search"
    :> APIKeyAuth v
    :> ReqBody '[JSON] OnSearchReq
    :> Post '[JSON] OnSearchRes

onSearchAPI :: Proxy (OnSearchAPI v)
onSearchAPI = Proxy

data SearchReq = SearchReq
  { context :: Context,
    message :: SearchIntent
  }
  deriving (Generic, Show, FromJSON, ToJSON)

type SearchRes = AckResponse

type OnSearchReq = CallbackReq OnSearchServices

newtype OnSearchServices = OnSearchServices
  { catalog :: Catalog
  }
  deriving (Generic, Show, FromJSON, ToJSON)

type OnSearchRes = AckResponse

newtype SearchIntent = SearchIntent
  { intent :: Intent
  }
  deriving (Generic, Show, ToJSON, FromJSON)
