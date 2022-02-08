{-# LANGUAGE TypeApplications #-}

module ExternalAPI where

import Beckn.Mock.App
import Beckn.Mock.ExternalAPI
import Beckn.Types.Core.Ack (AckResponse)
import Beckn.Types.Core.ReqTypes
import qualified Core.API.OnConfirm as BAP
import qualified Core.API.OnStatus as BAP
import qualified Core.OnConfirm as BAP
import Core.OnSearch
import qualified Core.OnStatus as BAP
import Environment
import Servant
import Servant.Client

type GatewayOnSearchAPI =
  "on_search"
    :> ReqBody '[JSON] (BecknCallbackReq OnSearchCatalog)
    :> Post '[JSON] AckResponse

callGatewayOnSearch :: BecknCallbackReq OnSearchCatalog -> MockM AppEnv ()
callGatewayOnSearch = callAPI @GatewayOnSearchAPI gatewayUrl
  where
    gatewayUrl = BaseUrl Http "localhost" 8015 "v1"

----------------------------
type OnConfirmAPI = BAP.OnConfirmAPI

callBapOnConfirm :: BecknCallbackReq BAP.OnConfirmMessage -> MockM AppEnv ()
callBapOnConfirm = callBapAPI @OnConfirmAPI

----------------------------
type OnStatusAPI = BAP.OnStatusAPI

callBapOnStatus :: BecknCallbackReq BAP.OnStatusMessage -> MockM AppEnv ()
callBapOnStatus = callBapAPI @OnStatusAPI
