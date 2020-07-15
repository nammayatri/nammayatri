{-# LANGUAGE DuplicateRecordFields #-}

module Beckn.Types.API.Search where

import Beckn.Types.Common
import Beckn.Types.Core.Context
import Beckn.Types.Mobility.Intent
import Beckn.Types.Mobility.Service
import Beckn.Utils.Common
import Data.Generics.Labels ()
import EulerHS.Prelude
import Servant (JSON, Post, ReqBody, (:>))

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

data OnSearchReq = OnSearchReq
  { context :: Context,
    message :: OnSearchServices
  }
  deriving (Generic, Show, FromJSON, ToJSON)

newtype OnSearchServices = OnSearchServices
  { services :: [Service]
  }
  deriving (Generic, Show, FromJSON, ToJSON)

type OnSearchRes = AckResponse

newtype SearchIntent = SearchIntent
  { intent :: Intent
  }
  deriving (Generic, Show, ToJSON, FromJSON)

instance Example OnSearchServices where
  example =
    OnSearchServices
      { services = example
      }
