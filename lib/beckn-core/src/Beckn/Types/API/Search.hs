{-# LANGUAGE DuplicateRecordFields #-}

module Beckn.Types.API.Search where

import Beckn.Types.Common
import Beckn.Types.Core.Context
import Beckn.Types.Mobility.Intent
import Beckn.Types.Mobility.Service
import Data.Generics.Labels ()
import EulerHS.Prelude
import Servant (JSON, Post, ReqBody, (:>))

type SearchAPI =
  "search"
    :> "services"
    :> ReqBody '[JSON] SearchReq
    :> Post '[JSON] AckResponse

searchAPI :: Proxy SearchAPI
searchAPI = Proxy

type OnSearchAPI =
  "on_search"
    :> "services"
    :> ReqBody '[JSON] OnSearchReq
    :> Post '[JSON] OnSearchRes

onSearchAPI :: Proxy OnSearchAPI
onSearchAPI = Proxy

data SearchReq = SearchReq
  { context :: Context,
    message :: Intent
  }
  deriving (Generic, Show, FromJSON, ToJSON)

type SearchRes = AckResponse

data OnSearchReq = OnSearchReq
  { context :: Context,
    message :: Service
  }
  deriving (Generic, Show, FromJSON, ToJSON)

type OnSearchRes = AckResponse
