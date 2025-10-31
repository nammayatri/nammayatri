{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.UI.Dispatcher
  ( API,
    handler,
  )
where

import qualified API.Types.UI.Dispatcher
import qualified Control.Lens
import qualified Domain.Action.UI.Dispatcher
import qualified Domain.Types.Merchant
import qualified Domain.Types.Person
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Storage.Beam.SystemConfigs ()
import Tools.Auth

type API =
  ( TokenAuth :> "dispatcher" :> Capture "fleetId" Kernel.Prelude.Text :> "getFleetInfo"
      :> Get
           ('[JSON])
           API.Types.UI.Dispatcher.DispatcherRes
      :<|> TokenAuth
      :> "dispatcher"
      :> "updateFleetSchedule"
      :> ReqBody ('[JSON]) API.Types.UI.Dispatcher.DispatcherReq
      :> Post
           ('[JSON])
           Kernel.Types.APISuccess.APISuccess
  )

handler :: Environment.FlowServer API
handler = getDispatcherGetFleetInfo :<|> postDispatcherUpdateFleetSchedule

getDispatcherGetFleetInfo ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Kernel.Prelude.Text ->
    Environment.FlowHandler API.Types.UI.Dispatcher.DispatcherRes
  )
getDispatcherGetFleetInfo a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.Dispatcher.getDispatcherGetFleetInfo (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a2) a1

postDispatcherUpdateFleetSchedule ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    API.Types.UI.Dispatcher.DispatcherReq ->
    Environment.FlowHandler Kernel.Types.APISuccess.APISuccess
  )
postDispatcherUpdateFleetSchedule a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.Dispatcher.postDispatcherUpdateFleetSchedule (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a2) a1
