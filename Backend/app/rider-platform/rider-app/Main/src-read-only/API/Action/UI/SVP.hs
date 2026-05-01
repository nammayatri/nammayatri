{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.UI.SVP
  ( API,
    handler,
  )
where

import qualified API.Types.UI.SVP
import qualified Control.Lens
import qualified Domain.Action.UI.SVP
import qualified Domain.Types.Merchant
import qualified Domain.Types.Person
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Storage.Beam.SystemConfigs ()
import Tools.Auth

type API =
  ( TokenAuth :> "svp" :> "qr" :> QueryParam "lat" Kernel.Prelude.Double :> QueryParam "lon" Kernel.Prelude.Double
      :> Get
           ('[JSON])
           API.Types.UI.SVP.GenerateQrResp
      :<|> "svp"
      :> "gate"
      :> ReqBody ('[JSON]) API.Types.UI.SVP.GateCallbackReq
      :> Post ('[JSON]) API.Types.UI.SVP.GateCallbackResp
  )

handler :: Environment.FlowServer API
handler = getSvpQr :<|> postSvpGate

getSvpQr ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Kernel.Prelude.Maybe (Kernel.Prelude.Double) ->
    Kernel.Prelude.Maybe (Kernel.Prelude.Double) ->
    Environment.FlowHandler API.Types.UI.SVP.GenerateQrResp
  )
getSvpQr a3 a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.SVP.getSvpQr (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a3) a2 a1

postSvpGate :: (API.Types.UI.SVP.GateCallbackReq -> Environment.FlowHandler API.Types.UI.SVP.GateCallbackResp)
postSvpGate a1 = withFlowHandlerAPI $ Domain.Action.UI.SVP.postSvpGate a1
