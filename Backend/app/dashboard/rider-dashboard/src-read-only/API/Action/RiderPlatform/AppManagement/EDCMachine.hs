{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.RiderPlatform.AppManagement.EDCMachine
  ( API,
    handler,
  )
where

import qualified API.Types.Dashboard.AppManagement
import qualified "rider-app" API.Types.Dashboard.AppManagement.EDCMachine
import qualified Data.Bool
import qualified Domain.Action.RiderPlatform.AppManagement.EDCMachine
import qualified "rider-app" Domain.Types.EDCMachineMapping
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified "rider-app" Domain.Types.Person
import qualified "lib-dashboard" Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Storage.Beam.CommonInstances ()
import Tools.Auth.Api

type API = ("edcMachine" :> (AssignEDCMachine :<|> ListEDCMachine :<|> UpdateEDCMachine :<|> DeleteEDCMachine))

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = assignEDCMachine merchantId city :<|> listEDCMachine merchantId city :<|> updateEDCMachine merchantId city :<|> deleteEDCMachine merchantId city

type AssignEDCMachine =
  ( ApiAuth
      ('APP_BACKEND_MANAGEMENT)
      ('DSL)
      (('RIDER_APP_MANAGEMENT) / ('API.Types.Dashboard.AppManagement.EDC_MACHINE) / ('API.Types.Dashboard.AppManagement.EDCMachine.ASSIGN_EDC_MACHINE))
      :> API.Types.Dashboard.AppManagement.EDCMachine.AssignEDCMachine
  )

type ListEDCMachine =
  ( ApiAuth
      ('APP_BACKEND_MANAGEMENT)
      ('DSL)
      (('RIDER_APP_MANAGEMENT) / ('API.Types.Dashboard.AppManagement.EDC_MACHINE) / ('API.Types.Dashboard.AppManagement.EDCMachine.LIST_EDC_MACHINE))
      :> API.Types.Dashboard.AppManagement.EDCMachine.ListEDCMachine
  )

type UpdateEDCMachine =
  ( ApiAuth
      ('APP_BACKEND_MANAGEMENT)
      ('DSL)
      (('RIDER_APP_MANAGEMENT) / ('API.Types.Dashboard.AppManagement.EDC_MACHINE) / ('API.Types.Dashboard.AppManagement.EDCMachine.UPDATE_EDC_MACHINE))
      :> API.Types.Dashboard.AppManagement.EDCMachine.UpdateEDCMachine
  )

type DeleteEDCMachine =
  ( ApiAuth
      ('APP_BACKEND_MANAGEMENT)
      ('DSL)
      (('RIDER_APP_MANAGEMENT) / ('API.Types.Dashboard.AppManagement.EDC_MACHINE) / ('API.Types.Dashboard.AppManagement.EDCMachine.DELETE_EDC_MACHINE))
      :> API.Types.Dashboard.AppManagement.EDCMachine.DeleteEDCMachine
  )

assignEDCMachine :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> API.Types.Dashboard.AppManagement.EDCMachine.AssignEDCMachineReq -> Environment.FlowHandler API.Types.Dashboard.AppManagement.EDCMachine.AssignEDCMachineResp)
assignEDCMachine merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.AppManagement.EDCMachine.assignEDCMachine merchantShortId opCity apiTokenInfo req

listEDCMachine :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Maybe (Data.Bool.Bool) -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person) -> Environment.FlowHandler API.Types.Dashboard.AppManagement.EDCMachine.EDCMachineMappingListResp)
listEDCMachine merchantShortId opCity apiTokenInfo isActive personId = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.AppManagement.EDCMachine.listEDCMachine merchantShortId opCity apiTokenInfo isActive personId

updateEDCMachine :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Domain.Types.EDCMachineMapping.EDCMachineMapping -> API.Types.Dashboard.AppManagement.EDCMachine.UpdateEDCMachineReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
updateEDCMachine merchantShortId opCity apiTokenInfo mappingId req = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.AppManagement.EDCMachine.updateEDCMachine merchantShortId opCity apiTokenInfo mappingId req

deleteEDCMachine :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Domain.Types.EDCMachineMapping.EDCMachineMapping -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
deleteEDCMachine merchantShortId opCity apiTokenInfo mappingId = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.AppManagement.EDCMachine.deleteEDCMachine merchantShortId opCity apiTokenInfo mappingId
