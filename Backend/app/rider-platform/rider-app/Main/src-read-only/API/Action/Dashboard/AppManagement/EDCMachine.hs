{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.Dashboard.AppManagement.EDCMachine
  ( API.Types.Dashboard.AppManagement.EDCMachine.API,
    handler,
  )
where

import qualified API.Types.Dashboard.AppManagement.EDCMachine
import qualified Data.Bool
import qualified Domain.Action.Dashboard.AppManagement.EDCMachine
import qualified "this" Domain.Types.EDCMachineMapping
import qualified Domain.Types.Merchant
import qualified "this" Domain.Types.Person
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Tools.Auth

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API.Types.Dashboard.AppManagement.EDCMachine.API)
handler merchantId city = assignEDCMachine merchantId city :<|> listEDCMachine merchantId city :<|> updateEDCMachine merchantId city :<|> deleteEDCMachine merchantId city

assignEDCMachine :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> API.Types.Dashboard.AppManagement.EDCMachine.AssignEDCMachineReq -> Environment.FlowHandler API.Types.Dashboard.AppManagement.EDCMachine.AssignEDCMachineResp)
assignEDCMachine a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.EDCMachine.assignEDCMachine a3 a2 a1

listEDCMachine :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Maybe (Data.Bool.Bool) -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person) -> Environment.FlowHandler API.Types.Dashboard.AppManagement.EDCMachine.EDCMachineMappingListResp)
listEDCMachine a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.EDCMachine.listEDCMachine a4 a3 a2 a1

updateEDCMachine :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Domain.Types.EDCMachineMapping.EDCMachineMapping -> API.Types.Dashboard.AppManagement.EDCMachine.UpdateEDCMachineReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
updateEDCMachine a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.EDCMachine.updateEDCMachine a4 a3 a2 a1

deleteEDCMachine :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Domain.Types.EDCMachineMapping.EDCMachineMapping -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
deleteEDCMachine a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.EDCMachine.deleteEDCMachine a3 a2 a1
