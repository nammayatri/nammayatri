{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module External.Gateway.Types where

import qualified Beckn.Types.API.Confirm as Confirm
import qualified Beckn.Types.API.Search as Search
import EulerHS.Language (Flow)
import EulerHS.Prelude
import EulerHS.Prelude
import qualified EulerHS.Types as ET
import EulerHS.Types (client)
import Servant

type ConfirmAPI =
  "confirm" :> "services" :> ReqBody '[JSON] Confirm.ConfirmReq :> Post '[JSON] Confirm.ConfirmRes

confirmAPI :: Proxy ConfirmAPI
confirmAPI = Proxy

confirm req =
  void $ ET.client confirmAPI req

type SearchAPI =
  "search"
    :> "services"
    :> ReqBody '[JSON] Search.SearchReq
    :> Post '[JSON] Search.SearchRes

searchAPI :: Proxy SearchAPI
searchAPI = Proxy

search req =
  void $ client searchAPI req
