{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module External.Gateway.Types where

import qualified Beckn.Types.API.Confirm as Confirm
import qualified Beckn.Types.API.Search as Search
import EulerHS.Language (Flow)
import EulerHS.Prelude
import EulerHS.Types (client)
import Servant

type ConfirmAPIs =
  "confirm" :> ReqBody '[JSON] Confirm.ConfirmReq :> Post '[JSON] Confirm.ConfirmRes

type SearchAPI =
  "search"
    :> "service"
    :> ReqBody '[JSON] Search.SearchReq
    :> Post '[JSON] Search.SearchRes

searchAPI :: Proxy SearchAPI
searchAPI = Proxy

search req =
  void $ client searchAPI req
