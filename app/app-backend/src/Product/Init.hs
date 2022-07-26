module Product.Init
  ( onInit,
  )
where

import App.Types
import Beckn.Prelude hiding (init)
import qualified Beckn.Types.Core.Taxi.API.OnInit as OnInit
import Beckn.Utils.Servant.SignatureAuth
import qualified Core.ACL.Confirm as ACL
import qualified Core.ACL.OnInit as TaxiACL
import qualified Domain.Action.Beckn.OnInit as DOnInit
import qualified ExternalAPI.Flow as ExternalAPI
import Utils.Common

onInit ::
  SignatureAuthResult ->
  OnInit.OnInitReq ->
  FlowHandler AckResponse
onInit _ req = withFlowHandlerBecknAPI . withTransactionIdLogTag req $ do
  mbDOnInitReq <- TaxiACL.buildOnInitReq req
  whenJust mbDOnInitReq $ \onInitReq -> do
    onInitRes <- DOnInit.onInit onInitReq
    void . ExternalAPI.confirm onInitRes.bppUrl =<< ACL.buildConfirmReq onInitRes
  pure Ack
