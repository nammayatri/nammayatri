{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.DashboardAuth.AppManagement.EDCMachine
  ( API,
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
import Tools.Auth.DashboardUserAuth

type API = ("edcMachine" :> (AssignEDCMachine :<|> ListEDCMachine :<|> UpdateEDCMachine :<|> DeleteEDCMachine))

type AssignEDCMachine = (DashboardUserAuth ('APP_BACKEND_MANAGEMENT) "RIDER_APP_MANAGEMENT/EDC_MACHINE/ASSIGN_EDC_MACHINE" :> API.Types.Dashboard.AppManagement.EDCMachine.AssignEDCMachine)

type ListEDCMachine = (DashboardUserAuth ('APP_BACKEND_MANAGEMENT) "RIDER_APP_MANAGEMENT/EDC_MACHINE/LIST_EDC_MACHINE" :> API.Types.Dashboard.AppManagement.EDCMachine.ListEDCMachine)

type UpdateEDCMachine = (DashboardUserAuth ('APP_BACKEND_MANAGEMENT) "RIDER_APP_MANAGEMENT/EDC_MACHINE/UPDATE_EDC_MACHINE" :> API.Types.Dashboard.AppManagement.EDCMachine.UpdateEDCMachine)

type DeleteEDCMachine = (DashboardUserAuth ('APP_BACKEND_MANAGEMENT) "RIDER_APP_MANAGEMENT/EDC_MACHINE/DELETE_EDC_MACHINE" :> API.Types.Dashboard.AppManagement.EDCMachine.DeleteEDCMachine)

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = assignEDCMachine merchantId city :<|> listEDCMachine merchantId city :<|> updateEDCMachine merchantId city :<|> deleteEDCMachine merchantId city

assignEDCMachine :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> API.Types.Dashboard.AppManagement.EDCMachine.AssignEDCMachineReq -> Environment.FlowHandler API.Types.Dashboard.AppManagement.EDCMachine.AssignEDCMachineResp)
assignEDCMachine a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.EDCMachine.assignEDCMachine a4 a3 a1

listEDCMachine :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (Data.Bool.Bool) -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person) -> Environment.FlowHandler API.Types.Dashboard.AppManagement.EDCMachine.EDCMachineMappingListResp)
listEDCMachine a5 a4 _a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.EDCMachine.listEDCMachine a5 a4 a2 a1

updateEDCMachine :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Domain.Types.EDCMachineMapping.EDCMachineMapping -> API.Types.Dashboard.AppManagement.EDCMachine.UpdateEDCMachineReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
updateEDCMachine a5 a4 _a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.EDCMachine.updateEDCMachine a5 a4 a2 a1

deleteEDCMachine :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Domain.Types.EDCMachineMapping.EDCMachineMapping -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
deleteEDCMachine a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.EDCMachine.deleteEDCMachine a4 a3 a1
