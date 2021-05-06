{-# LANGUAGE DuplicateRecordFields #-}

module Types.Beckn.API.Search where

import Data.Generics.Labels ()
import EulerHS.Prelude
import Servant (JSON, Post, ReqBody, (:>))
import Types.Beckn.API.Callback
import Types.Beckn.Ack (AckResponse (..))
import Types.Beckn.Catalog
import Types.Beckn.Context
import Types.Beckn.Intent

type SearchAPI =
  "search"
    :> ReqBody '[JSON] SearchReq
    :> Post '[JSON] AckResponse

searchAPI :: Proxy SearchAPI
searchAPI = Proxy

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
