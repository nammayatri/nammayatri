module External.Gateway.API where

import Beckn.Types.API.Confirm as Confirm
import Beckn.Types.API.Search
import EulerHS.Prelude
import qualified EulerHS.Types as ET
import External.Gateway.Types
import Servant
import Servant.API.ContentTypes
import Servant.Client

type SearchAPI =
  "on_search"
    :> "services"
    :> ReqBody '[JSON] OnSearchReq
    :> Post '[JSON] OnSearchRes

searchAPI :: Proxy SearchAPI
searchAPI = Proxy

onSearch req =
  void $ ET.client searchAPI req

type ConfirmAPI =
  "on_confirm"
    :> "services"
    :> ReqBody '[JSON] Confirm.OnConfirmReq
    :> Post '[JSON] Confirm.OnConfirmRes

confirmAPI :: Proxy ConfirmAPI
confirmAPI = Proxy

onConfirm req =
  void $ ET.client confirmAPI req
