{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.RiderPlatform.AppManagement.PassOrganization
  ( API,
    handler,
  )
where
import EulerHS.Prelude
import Servant
import Tools.Auth.Api
import Kernel.Utils.Common
import Storage.Beam.CommonInstances ()
import qualified "rider-app" API.Types.Dashboard.AppManagement.PassOrganization
import qualified API.Types.Dashboard.AppManagement
import qualified Domain.Action.RiderPlatform.AppManagement.PassOrganization
import qualified "rider-app" Domain.Types.Person
import qualified Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Kernel.Types.Beckn.Context
import qualified "rider-app" Domain.Types.PassOrganization
import qualified "rider-app" Domain.Types.PassDetails
import qualified Kernel.Types.APISuccess
import qualified "rider-app" Domain.Types.PassType
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified "lib-dashboard" Environment


type API = ("passOrganization" :> (GetPassOrganizationGetPassOrganizationId :<|> GetPassOrganizationPassDetails :<|> PostPassOrganizationPassDetailsVerify :<|> PostPassOrganizationUpdate :<|> GetPassOrganizationGetOrganizations))
handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = getPassOrganizationGetPassOrganizationId merchantId city
  :<|> getPassOrganizationPassDetails merchantId city
  :<|> postPassOrganizationPassDetailsVerify merchantId city
  :<|> postPassOrganizationUpdate merchantId city
  :<|> getPassOrganizationGetOrganizations merchantId city

type GetPassOrganizationGetPassOrganizationId =
  ( ApiAuth
      ('APP_BACKEND_MANAGEMENT)
      ('DSL)
      (('RIDER_APP_MANAGEMENT) / ('API.Types.Dashboard.AppManagement.PASS_ORGANIZATION) / ('API.Types.Dashboard.AppManagement.PassOrganization.GET_PASS_ORGANIZATION_GET_PASS_ORGANIZATION_ID))
      :> API.Types.Dashboard.AppManagement.PassOrganization.GetPassOrganizationGetPassOrganizationId
  )

type GetPassOrganizationPassDetails =
  ( ApiAuth
      ('APP_BACKEND_MANAGEMENT)
      ('DSL)
      (('RIDER_APP_MANAGEMENT) / ('API.Types.Dashboard.AppManagement.PASS_ORGANIZATION) / ('API.Types.Dashboard.AppManagement.PassOrganization.GET_PASS_ORGANIZATION_PASS_DETAILS))
      :> API.Types.Dashboard.AppManagement.PassOrganization.GetPassOrganizationPassDetails
  )

type PostPassOrganizationPassDetailsVerify =
  ( ApiAuth
      ('APP_BACKEND_MANAGEMENT)
      ('DSL)
      (('RIDER_APP_MANAGEMENT) / ('API.Types.Dashboard.AppManagement.PASS_ORGANIZATION) / ('API.Types.Dashboard.AppManagement.PassOrganization.POST_PASS_ORGANIZATION_PASS_DETAILS_VERIFY))
      :> API.Types.Dashboard.AppManagement.PassOrganization.PostPassOrganizationPassDetailsVerify
  )

type PostPassOrganizationUpdate =
  ( ApiAuth
      ('APP_BACKEND_MANAGEMENT)
      ('DSL)
      (('RIDER_APP_MANAGEMENT) / ('API.Types.Dashboard.AppManagement.PASS_ORGANIZATION) / ('API.Types.Dashboard.AppManagement.PassOrganization.POST_PASS_ORGANIZATION_UPDATE))
      :> API.Types.Dashboard.AppManagement.PassOrganization.PostPassOrganizationUpdate
  )

type GetPassOrganizationGetOrganizations =
  ( ApiAuth
      ('APP_BACKEND_MANAGEMENT)
      ('DSL)
      (('RIDER_APP_MANAGEMENT) / ('API.Types.Dashboard.AppManagement.PASS_ORGANIZATION) / ('API.Types.Dashboard.AppManagement.PassOrganization.GET_PASS_ORGANIZATION_GET_ORGANIZATIONS))
      :> API.Types.Dashboard.AppManagement.PassOrganization.GetPassOrganizationGetOrganizations
  )

getPassOrganizationGetPassOrganizationId :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Domain.Types.Person.Person -> Environment.FlowHandler (Kernel.Types.Id.Id Domain.Types.PassOrganization.PassOrganization))
getPassOrganizationGetPassOrganizationId merchantShortId opCity apiTokenInfo personId = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.AppManagement.PassOrganization.getPassOrganizationGetPassOrganizationId merchantShortId opCity apiTokenInfo personId

getPassOrganizationPassDetails :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Domain.Types.PassOrganization.PassOrganization -> Kernel.Prelude.Maybe (Domain.Types.PassDetails.VerificationStatus) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Environment.FlowHandler API.Types.Dashboard.AppManagement.PassOrganization.PassDetailsListResp)
getPassOrganizationPassDetails merchantShortId opCity apiTokenInfo passOrganizationId status limit offset = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.AppManagement.PassOrganization.getPassOrganizationPassDetails merchantShortId opCity apiTokenInfo passOrganizationId status limit offset

postPassOrganizationPassDetailsVerify :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> API.Types.Dashboard.AppManagement.PassOrganization.VerifyPassDetailsReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postPassOrganizationPassDetailsVerify merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.AppManagement.PassOrganization.postPassOrganizationPassDetailsVerify merchantShortId opCity apiTokenInfo req

postPassOrganizationUpdate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Domain.Types.Person.Person -> API.Types.Dashboard.AppManagement.PassOrganization.PassOrganizationUpdateReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postPassOrganizationUpdate merchantShortId opCity apiTokenInfo personId req = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.AppManagement.PassOrganization.postPassOrganizationUpdate merchantShortId opCity apiTokenInfo personId req

getPassOrganizationGetOrganizations :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Domain.Types.PassType.PassEnum -> Environment.FlowHandler [API.Types.Dashboard.AppManagement.PassOrganization.GetOrganizationResp])
getPassOrganizationGetOrganizations merchantShortId opCity apiTokenInfo passEnum = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.AppManagement.PassOrganization.getPassOrganizationGetOrganizations merchantShortId opCity apiTokenInfo passEnum