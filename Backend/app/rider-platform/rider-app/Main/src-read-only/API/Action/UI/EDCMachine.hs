{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.UI.EDCMachine
  ( API,
    handler,
  )
where

import qualified API.Types.UI.EDCMachine
import qualified Control.Lens
import qualified Domain.Action.UI.EDCMachine
import qualified Domain.Types.EDCMachineMapping
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
  ( TokenAuth :> "edcMachine" :> "assign" :> ReqBody '[JSON] API.Types.UI.EDCMachine.AssignEDCMachineReq
      :> Post
           '[JSON]
           API.Types.UI.EDCMachine.AssignEDCMachineResp
      :<|> TokenAuth
      :> "edcMachine"
      :> "list"
      :> QueryParam
           "personId"
           (Kernel.Types.Id.Id Domain.Types.Person.Person)
      :> QueryParam
           "isActive"
           Kernel.Prelude.Bool
      :> Get
           '[JSON]
           API.Types.UI.EDCMachine.EDCMachineMappingListResp
      :<|> TokenAuth
      :> "edcMachine"
      :> Capture
           "mappingId"
           (Kernel.Types.Id.Id Domain.Types.EDCMachineMapping.EDCMachineMapping)
      :> "update"
      :> ReqBody '[JSON] API.Types.UI.EDCMachine.UpdateEDCMachineReq
      :> Put
           '[JSON]
           Kernel.Types.APISuccess.APISuccess
      :<|> TokenAuth
      :> "edcMachine"
      :> Capture
           "mappingId"
           (Kernel.Types.Id.Id Domain.Types.EDCMachineMapping.EDCMachineMapping)
      :> "delete"
      :> Delete
           '[JSON]
           Kernel.Types.APISuccess.APISuccess
  )

handler :: Environment.FlowServer API
handler = postEdcMachineAssign :<|> getEdcMachineList :<|> putEdcMachineUpdate :<|> deleteEdcMachineDelete

postEdcMachineAssign ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    API.Types.UI.EDCMachine.AssignEDCMachineReq ->
    Environment.FlowHandler API.Types.UI.EDCMachine.AssignEDCMachineResp
  )
postEdcMachineAssign a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.EDCMachine.postEdcMachineAssign (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a2) a1

getEdcMachineList ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person) ->
    Kernel.Prelude.Maybe Kernel.Prelude.Bool ->
    Environment.FlowHandler API.Types.UI.EDCMachine.EDCMachineMappingListResp
  )
getEdcMachineList a3 a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.EDCMachine.getEdcMachineList (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a3) a2 a1

putEdcMachineUpdate ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Kernel.Types.Id.Id Domain.Types.EDCMachineMapping.EDCMachineMapping ->
    API.Types.UI.EDCMachine.UpdateEDCMachineReq ->
    Environment.FlowHandler Kernel.Types.APISuccess.APISuccess
  )
putEdcMachineUpdate a3 a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.EDCMachine.putEdcMachineUpdate (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a3) a2 a1

deleteEdcMachineDelete ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Kernel.Types.Id.Id Domain.Types.EDCMachineMapping.EDCMachineMapping ->
    Environment.FlowHandler Kernel.Types.APISuccess.APISuccess
  )
deleteEdcMachineDelete a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.EDCMachine.deleteEdcMachineDelete (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a2) a1
