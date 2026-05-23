{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.RiderPlatform.AppManagement.PassOrganization
  ( API,
    handler,
  )
where

import qualified API.Types.Dashboard.AppManagement
import qualified "rider-app" API.Types.Dashboard.AppManagement.PassOrganization
import qualified Domain.Action.RiderPlatform.AppManagement.PassOrganization
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified "rider-app" Domain.Types.PassOrganization
import qualified "rider-app" Domain.Types.Person
import qualified "lib-dashboard" Environment
import EulerHS.Prelude
import qualified IssueManagement.Domain.Types.MediaFile
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Storage.Beam.CommonInstances ()
import Tools.Auth.Api

type API = ("passOrganization" :> (GetPassOrganizationGetPassOrganization :<|> GetPassOrganizationPassDetails :<|> PostPassOrganizationPassDetailsVerify :<|> PostPassOrganizationUpdate :<|> GetPassOrganizationGetOrganizations :<|> GetPassOrganizationPassDetailsDocument :<|> PostPassOrganizationAssignDepot))

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = getPassOrganizationGetPassOrganization merchantId city :<|> getPassOrganizationPassDetails merchantId city :<|> postPassOrganizationPassDetailsVerify merchantId city :<|> postPassOrganizationUpdate merchantId city :<|> getPassOrganizationGetOrganizations merchantId city :<|> getPassOrganizationPassDetailsDocument merchantId city :<|> postPassOrganizationAssignDepot merchantId city

type GetPassOrganizationGetPassOrganization =
  ( ApiAuth
      'APP_BACKEND_MANAGEMENT
      'DSL
      ('RIDER_APP_MANAGEMENT / 'API.Types.Dashboard.AppManagement.PASS_ORGANIZATION / 'API.Types.Dashboard.AppManagement.PassOrganization.GET_PASS_ORGANIZATION_GET_PASS_ORGANIZATION)
      :> API.Types.Dashboard.AppManagement.PassOrganization.GetPassOrganizationGetPassOrganization
  )

type GetPassOrganizationPassDetails =
  ( ApiAuth
      'APP_BACKEND_MANAGEMENT
      'DSL
      ('RIDER_APP_MANAGEMENT / 'API.Types.Dashboard.AppManagement.PASS_ORGANIZATION / 'API.Types.Dashboard.AppManagement.PassOrganization.GET_PASS_ORGANIZATION_PASS_DETAILS)
      :> API.Types.Dashboard.AppManagement.PassOrganization.GetPassOrganizationPassDetails
  )

type PostPassOrganizationPassDetailsVerify =
  ( ApiAuth
      'APP_BACKEND_MANAGEMENT
      'DSL
      ('RIDER_APP_MANAGEMENT / 'API.Types.Dashboard.AppManagement.PASS_ORGANIZATION / 'API.Types.Dashboard.AppManagement.PassOrganization.POST_PASS_ORGANIZATION_PASS_DETAILS_VERIFY)
      :> API.Types.Dashboard.AppManagement.PassOrganization.PostPassOrganizationPassDetailsVerify
  )

type PostPassOrganizationUpdate =
  ( ApiAuth
      'APP_BACKEND_MANAGEMENT
      'DSL
      ('RIDER_APP_MANAGEMENT / 'API.Types.Dashboard.AppManagement.PASS_ORGANIZATION / 'API.Types.Dashboard.AppManagement.PassOrganization.POST_PASS_ORGANIZATION_UPDATE)
      :> API.Types.Dashboard.AppManagement.PassOrganization.PostPassOrganizationUpdate
  )

type GetPassOrganizationGetOrganizations =
  ( ApiAuth
      'APP_BACKEND_MANAGEMENT
      'DSL
      ('RIDER_APP_MANAGEMENT / 'API.Types.Dashboard.AppManagement.PASS_ORGANIZATION / 'API.Types.Dashboard.AppManagement.PassOrganization.GET_PASS_ORGANIZATION_GET_ORGANIZATIONS)
      :> API.Types.Dashboard.AppManagement.PassOrganization.GetPassOrganizationGetOrganizations
  )

type GetPassOrganizationPassDetailsDocument =
  ( ApiAuth
      'APP_BACKEND_MANAGEMENT
      'DSL
      ('RIDER_APP_MANAGEMENT / 'API.Types.Dashboard.AppManagement.PASS_ORGANIZATION / 'API.Types.Dashboard.AppManagement.PassOrganization.GET_PASS_ORGANIZATION_PASS_DETAILS_DOCUMENT)
      :> API.Types.Dashboard.AppManagement.PassOrganization.GetPassOrganizationPassDetailsDocument
  )

type PostPassOrganizationAssignDepot =
  ( ApiAuth
      'APP_BACKEND_MANAGEMENT
      'DSL
      ('RIDER_APP_MANAGEMENT / 'API.Types.Dashboard.AppManagement.PASS_ORGANIZATION / 'API.Types.Dashboard.AppManagement.PassOrganization.POST_PASS_ORGANIZATION_ASSIGN_DEPOT)
      :> API.Types.Dashboard.AppManagement.PassOrganization.PostPassOrganizationAssignDepot
  )

getPassOrganizationGetPassOrganization :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Domain.Types.Person.Person -> Environment.FlowHandler API.Types.Dashboard.AppManagement.PassOrganization.GetOrganizationResp)
getPassOrganizationGetPassOrganization merchantShortId opCity apiTokenInfo personId = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.AppManagement.PassOrganization.getPassOrganizationGetPassOrganization merchantShortId opCity apiTokenInfo personId

getPassOrganizationPassDetails :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.PassOrganization.PassOrganization) -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Environment.FlowHandler API.Types.Dashboard.AppManagement.PassOrganization.PassDetailsListResp)
getPassOrganizationPassDetails merchantShortId opCity apiTokenInfo passEnum passOrganizationId status limit offset = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.AppManagement.PassOrganization.getPassOrganizationPassDetails merchantShortId opCity apiTokenInfo passEnum passOrganizationId status limit offset

postPassOrganizationPassDetailsVerify :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> API.Types.Dashboard.AppManagement.PassOrganization.VerifyPassDetailsReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postPassOrganizationPassDetailsVerify merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.AppManagement.PassOrganization.postPassOrganizationPassDetailsVerify merchantShortId opCity apiTokenInfo req

postPassOrganizationUpdate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Domain.Types.Person.Person -> API.Types.Dashboard.AppManagement.PassOrganization.PassOrganizationUpdateReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postPassOrganizationUpdate merchantShortId opCity apiTokenInfo personId req = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.AppManagement.PassOrganization.postPassOrganizationUpdate merchantShortId opCity apiTokenInfo personId req

getPassOrganizationGetOrganizations :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person) -> Environment.FlowHandler [API.Types.Dashboard.AppManagement.PassOrganization.GetOrganizationResp])
getPassOrganizationGetOrganizations merchantShortId opCity apiTokenInfo passEnum depotPersonId = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.AppManagement.PassOrganization.getPassOrganizationGetOrganizations merchantShortId opCity apiTokenInfo passEnum depotPersonId

getPassOrganizationPassDetailsDocument :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id IssueManagement.Domain.Types.MediaFile.MediaFile -> Environment.FlowHandler Kernel.Prelude.Text)
getPassOrganizationPassDetailsDocument merchantShortId opCity apiTokenInfo documentId = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.AppManagement.PassOrganization.getPassOrganizationPassDetailsDocument merchantShortId opCity apiTokenInfo documentId

postPassOrganizationAssignDepot :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> API.Types.Dashboard.AppManagement.PassOrganization.AssignDepotReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postPassOrganizationAssignDepot merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.AppManagement.PassOrganization.postPassOrganizationAssignDepot merchantShortId opCity apiTokenInfo req
