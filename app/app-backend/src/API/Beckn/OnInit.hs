module API.Beckn.OnInit (API, handler) where

import App.Types
import Beckn.Prelude
import qualified Beckn.Types.Core.Taxi.API.OnInit as OnInit
import Beckn.Utils.Common
import Beckn.Utils.Servant.SignatureAuth
import qualified Core.ACL.Confirm as ACL
import qualified Core.ACL.OnInit as TaxiACL
import qualified Domain.Action.Beckn.OnInit as DOnInit
import qualified SharedLogic.CallBPP as CallBPP

type API = OnInit.OnInitAPI

handler :: SignatureAuthResult -> FlowServer API
handler = onInit

onInit ::
  SignatureAuthResult ->
  OnInit.OnInitReq ->
  FlowHandler AckResponse
onInit (SignatureAuthResult _ _ registryUrl) req = withFlowHandlerBecknAPI . withTransactionIdLogTag req $ do
  mbDOnInitReq <- TaxiACL.buildOnInitReq req
  whenJust mbDOnInitReq $ \onInitReq -> do
    onInitRes <- DOnInit.onInit registryUrl onInitReq
    void . CallBPP.confirm onInitRes.bppUrl =<< ACL.buildConfirmReq onInitRes
  pure Ack
