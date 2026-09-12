{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.DashboardAuth.AppManagement.PassOrganization
  ( API,
    handler,
  )
where

import qualified API.Types.Dashboard.AppManagement.PassOrganization
import qualified Domain.Action.Dashboard.AppManagement.PassOrganization
import qualified Domain.Types.Merchant
import qualified "this" Domain.Types.PassOrganization
import qualified "this" Domain.Types.Person
import qualified Environment
import EulerHS.Prelude
import qualified IssueManagement.Domain.Types.MediaFile
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Tools.Auth
import Tools.Auth.DashboardUserAuth

type API = ("passOrganization" :> (GetPassOrganizationGetPassOrganization :<|> GetPassOrganizationPassDetailsDepot :<|> GetPassOrganizationPassDetails :<|> PostPassOrganizationPassDetailsVerify :<|> PostPassOrganizationUpdate :<|> GetPassOrganizationGetOrganizations :<|> GetPassOrganizationPassDetailsDocument :<|> PostPassOrganizationAssignDepot))

type GetPassOrganizationGetPassOrganization =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/PASS_ORGANIZATION/GET_PASS_ORGANIZATION_GET_PASS_ORGANIZATION"
      :> API.Types.Dashboard.AppManagement.PassOrganization.GetPassOrganizationGetPassOrganization
  )

type GetPassOrganizationPassDetailsDepot =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/PASS_ORGANIZATION/GET_PASS_ORGANIZATION_PASS_DETAILS_DEPOT"
      :> API.Types.Dashboard.AppManagement.PassOrganization.GetPassOrganizationPassDetailsDepot
  )

type GetPassOrganizationPassDetails =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/PASS_ORGANIZATION/GET_PASS_ORGANIZATION_PASS_DETAILS"
      :> API.Types.Dashboard.AppManagement.PassOrganization.GetPassOrganizationPassDetails
  )

type PostPassOrganizationPassDetailsVerify =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/PASS_ORGANIZATION/POST_PASS_ORGANIZATION_PASS_DETAILS_VERIFY"
      :> API.Types.Dashboard.AppManagement.PassOrganization.PostPassOrganizationPassDetailsVerify
  )

type PostPassOrganizationUpdate =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/PASS_ORGANIZATION/POST_PASS_ORGANIZATION_UPDATE"
      :> API.Types.Dashboard.AppManagement.PassOrganization.PostPassOrganizationUpdate
  )

type GetPassOrganizationGetOrganizations =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/PASS_ORGANIZATION/GET_PASS_ORGANIZATION_GET_ORGANIZATIONS"
      :> API.Types.Dashboard.AppManagement.PassOrganization.GetPassOrganizationGetOrganizations
  )

type GetPassOrganizationPassDetailsDocument =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/PASS_ORGANIZATION/GET_PASS_ORGANIZATION_PASS_DETAILS_DOCUMENT"
      :> API.Types.Dashboard.AppManagement.PassOrganization.GetPassOrganizationPassDetailsDocument
  )

type PostPassOrganizationAssignDepot =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/PASS_ORGANIZATION/POST_PASS_ORGANIZATION_ASSIGN_DEPOT"
      :> API.Types.Dashboard.AppManagement.PassOrganization.PostPassOrganizationAssignDepot
  )

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = getPassOrganizationGetPassOrganization merchantId city :<|> getPassOrganizationPassDetailsDepot merchantId city :<|> getPassOrganizationPassDetails merchantId city :<|> postPassOrganizationPassDetailsVerify merchantId city :<|> postPassOrganizationUpdate merchantId city :<|> getPassOrganizationGetOrganizations merchantId city :<|> getPassOrganizationPassDetailsDocument merchantId city :<|> postPassOrganizationAssignDepot merchantId city

getPassOrganizationGetPassOrganization :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Domain.Types.Person.Person -> Environment.FlowHandler API.Types.Dashboard.AppManagement.PassOrganization.GetOrganizationResp)
getPassOrganizationGetPassOrganization a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.PassOrganization.getPassOrganizationGetPassOrganization a4 a3 a1

getPassOrganizationPassDetailsDepot :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Types.Id.Id Domain.Types.Person.Person -> Kernel.Prelude.Text -> Environment.FlowHandler API.Types.Dashboard.AppManagement.PassOrganization.PassDetailsListResp)
getPassOrganizationPassDetailsDepot a7 a6 _a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.PassOrganization.getPassOrganizationPassDetailsDepot a7 a6 a4 a3 a2 a1

getPassOrganizationPassDetails :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.PassOrganization.PassOrganization) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Text -> Environment.FlowHandler API.Types.Dashboard.AppManagement.PassOrganization.PassDetailsListResp)
getPassOrganizationPassDetails a8 a7 _a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.PassOrganization.getPassOrganizationPassDetails a8 a7 a5 a4 a3 a2 a1

postPassOrganizationPassDetailsVerify :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> API.Types.Dashboard.AppManagement.PassOrganization.VerifyPassDetailsReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postPassOrganizationPassDetailsVerify a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.PassOrganization.postPassOrganizationPassDetailsVerify a4 a3 a1

postPassOrganizationUpdate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Domain.Types.Person.Person -> API.Types.Dashboard.AppManagement.PassOrganization.PassOrganizationUpdateReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postPassOrganizationUpdate a5 a4 _a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.PassOrganization.postPassOrganizationUpdate a5 a4 a2 a1

getPassOrganizationGetOrganizations :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person) -> Environment.FlowHandler [API.Types.Dashboard.AppManagement.PassOrganization.GetOrganizationResp])
getPassOrganizationGetOrganizations a5 a4 _a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.PassOrganization.getPassOrganizationGetOrganizations a5 a4 a2 a1

getPassOrganizationPassDetailsDocument :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id IssueManagement.Domain.Types.MediaFile.MediaFile -> Environment.FlowHandler Kernel.Prelude.Text)
getPassOrganizationPassDetailsDocument a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.PassOrganization.getPassOrganizationPassDetailsDocument a4 a3 a1

postPassOrganizationAssignDepot :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> API.Types.Dashboard.AppManagement.PassOrganization.AssignDepotReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postPassOrganizationAssignDepot a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.PassOrganization.postPassOrganizationAssignDepot a4 a3 a1
