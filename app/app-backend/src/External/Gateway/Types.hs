{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module External.Gateway.Types where

import qualified Beckn.Types.API.Confirm as Confirm
import EulerHS.Prelude
import qualified EulerHS.Types as ET
import qualified Beckn.Types.API.Search as Search
import EulerHS.Language (Flow)
import EulerHS.Prelude
import EulerHS.Types (client)
import Servant

type ConfirmAPI =
  "confirm" :> ReqBody '[JSON] Confirm.ConfirmReq :> Post '[JSON] Confirm.ConfirmRes

confirmAPI :: Proxy ConfirmAPI
confirmAPI = Proxy

confirm req =
  void $ ET.client confirmAPI req

type SearchAPI =
  "search"
    :> "service"
    :> ReqBody '[JSON] Search.SearchReq
    :> Post '[JSON] Search.SearchRes

searchAPI :: Proxy SearchAPI
searchAPI = Proxy

search req =
  void $ client searchAPI req
