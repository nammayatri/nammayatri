{-# LANGUAGE TypeApplications #-}

module ExternalAPI where

import "public-transport-bap" Beckn.Spec.OnCancel
import "public-transport-bap" Beckn.Spec.OnConfirm
import "public-transport-bap" Beckn.Spec.OnSearch
import "public-transport-bap" Beckn.Spec.OnStatus
import Environment
import Kernel.Mock.App
import Kernel.Mock.ExternalAPI
import Kernel.Types.Beckn.Ack (AckResponse)
import Kernel.Types.Beckn.ReqTypes
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
type OnConfirmAPI =
  "on_confirm"
    :> ReqBody '[JSON] (BecknCallbackReq OnConfirmMessage)
    :> Post '[JSON] AckResponse

callBapOnConfirm :: BecknCallbackReq OnConfirmMessage -> MockM AppEnv ()
callBapOnConfirm = callBapAPI @OnConfirmAPI

----------------------------
type OnStatusAPI =
  "on_status"
    :> ReqBody '[JSON] (BecknCallbackReq OnStatusMessage)
    :> Post '[JSON] AckResponse

callBapOnStatus :: BecknCallbackReq OnStatusMessage -> MockM AppEnv ()
callBapOnStatus = callBapAPI @OnStatusAPI

----------------------------
type OnCancelAPI =
  "on_cancel"
    :> ReqBody '[JSON] (BecknCallbackReq OnCancelMessage)
    :> Post '[JSON] AckResponse

callBapOnCancel :: BecknCallbackReq OnCancelMessage -> MockM AppEnv ()
callBapOnCancel = callBapAPI @OnCancelAPI
