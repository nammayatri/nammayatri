module Product.OnInit where

import App.Types
import Beckn.Prelude
import qualified Beckn.Types.Core.Taxi.API.OnInit as OnInit
import Beckn.Types.Id
import qualified Core.ACL.OnInit as ACL
import qualified Domain.Action.Beckn.OnInit as DOnInit
import qualified Domain.Types.Person as SP
import Utils.Common

onInitFlow ::
  Id SP.Person ->
  OnInit.OnInitReq ->
  FlowHandler OnInit.OnInitRes
onInitFlow _ req =
  withFlowHandlerAPI $ do
    mbOnInitReq <- ACL.buildOnInitReq req
    whenJust mbOnInitReq DOnInit.onInit
    return Ack
