{-# LANGUAGE DuplicateRecordFields #-}

module Beckn.Types.Core.API.Search
  ( module Beckn.Types.Core.API.Search,
    module Beckn.Types.Core.API.Callback,
  )
where

import Beckn.Types.Core.API.Callback
import Beckn.Types.Core.Ack
import Beckn.Types.Core.Context
import Beckn.Types.Mobility.Catalog
import Beckn.Types.Mobility.Intent
import Beckn.Utils.Example
import Data.Generics.Labels ()
import EulerHS.Prelude
import Servant (JSON, Post, ReqBody, (:>))

type SearchAPI =
  "search"
    :> ReqBody '[JSON] SearchReq
    :> Post '[JSON] AckResponse

searchAPI :: Proxy SearchAPI
searchAPI = Proxy

type NSDLSearchAPI =
  "search"
    :> ReqBody '[JSON] SearchReq
    :> Post '[JSON] AckResponse

nsdlSearchAPI :: Proxy NSDLSearchAPI
nsdlSearchAPI = Proxy

type OnSearchAPI =
  "on_search"
    :> ReqBody '[JSON] OnSearchReq
    :> Post '[JSON] OnSearchRes

onSearchAPI :: Proxy OnSearchAPI
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

instance Example SearchIntent where
  example = SearchIntent example

instance Example OnSearchServices where
  example =
    OnSearchServices
      { catalog = example
      }

type OnSearchEndAPI =
  "on_search"
    :> "end"
    :> ReqBody '[JSON] OnSearchEndReq
    :> Post '[JSON] OnSearchEndRes

onSearchEndAPI :: Proxy OnSearchEndAPI
onSearchEndAPI = Proxy

newtype OnSearchEndReq = OnSearchEndReq {context :: Context}
  deriving (Generic, Show, FromJSON, ToJSON)

type OnSearchEndRes = AckResponse
