{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.RiderPlatform.AppManagement.Organization
  ( API,
    handler,
  )
where

import qualified API.Types.Dashboard.AppManagement
import qualified "rider-app" API.Types.Dashboard.AppManagement.Organization
import qualified Domain.Action.RiderPlatform.AppManagement.Organization
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified "rider-app" Domain.Types.Organization
import qualified "rider-app" Domain.Types.Person
import qualified "rider-app" Domain.Types.Student
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

type API = ("organization" :> (GetOrganizationGetOrganizationId :<|> GetOrganizationStudentOrganization :<|> PostOrganizationStudentVerify :<|> PostOrganizationOrganizationUpdate))

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = getOrganizationGetOrganizationId merchantId city :<|> getOrganizationStudentOrganization merchantId city :<|> postOrganizationStudentVerify merchantId city :<|> postOrganizationOrganizationUpdate merchantId city

type GetOrganizationGetOrganizationId =
  ( ApiAuth
      ('APP_BACKEND_MANAGEMENT)
      ('DSL)
      (('RIDER_APP_MANAGEMENT) / ('API.Types.Dashboard.AppManagement.ORGANIZATION) / ('API.Types.Dashboard.AppManagement.Organization.GET_ORGANIZATION_GET_ORGANIZATION_ID))
      :> API.Types.Dashboard.AppManagement.Organization.GetOrganizationGetOrganizationId
  )

type GetOrganizationStudentOrganization =
  ( ApiAuth
      ('APP_BACKEND_MANAGEMENT)
      ('DSL)
      (('RIDER_APP_MANAGEMENT) / ('API.Types.Dashboard.AppManagement.ORGANIZATION) / ('API.Types.Dashboard.AppManagement.Organization.GET_ORGANIZATION_STUDENT_ORGANIZATION))
      :> API.Types.Dashboard.AppManagement.Organization.GetOrganizationStudentOrganization
  )

type PostOrganizationStudentVerify =
  ( ApiAuth
      ('APP_BACKEND_MANAGEMENT)
      ('DSL)
      (('RIDER_APP_MANAGEMENT) / ('API.Types.Dashboard.AppManagement.ORGANIZATION) / ('API.Types.Dashboard.AppManagement.Organization.POST_ORGANIZATION_STUDENT_VERIFY))
      :> API.Types.Dashboard.AppManagement.Organization.PostOrganizationStudentVerify
  )

type PostOrganizationOrganizationUpdate =
  ( ApiAuth
      ('APP_BACKEND_MANAGEMENT)
      ('DSL)
      (('RIDER_APP_MANAGEMENT) / ('API.Types.Dashboard.AppManagement.ORGANIZATION) / ('API.Types.Dashboard.AppManagement.Organization.POST_ORGANIZATION_ORGANIZATION_UPDATE))
      :> API.Types.Dashboard.AppManagement.Organization.PostOrganizationOrganizationUpdate
  )

getOrganizationGetOrganizationId :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Domain.Types.Person.Person -> Environment.FlowHandler (Kernel.Types.Id.Id Domain.Types.Organization.Organization))
getOrganizationGetOrganizationId merchantShortId opCity apiTokenInfo personId = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.AppManagement.Organization.getOrganizationGetOrganizationId merchantShortId opCity apiTokenInfo personId

getOrganizationStudentOrganization :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Domain.Types.Organization.Organization -> Kernel.Prelude.Maybe (Domain.Types.Student.VerificationStatus) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Environment.FlowHandler API.Types.Dashboard.AppManagement.Organization.StudentsListResp)
getOrganizationStudentOrganization merchantShortId opCity apiTokenInfo organizationId status limit offset = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.AppManagement.Organization.getOrganizationStudentOrganization merchantShortId opCity apiTokenInfo organizationId status limit offset

postOrganizationStudentVerify :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> API.Types.Dashboard.AppManagement.Organization.VerifyStudentsReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postOrganizationStudentVerify merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.AppManagement.Organization.postOrganizationStudentVerify merchantShortId opCity apiTokenInfo req

postOrganizationOrganizationUpdate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Domain.Types.Person.Person -> API.Types.Dashboard.AppManagement.Organization.OrganizationUpdateReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postOrganizationOrganizationUpdate merchantShortId opCity apiTokenInfo personId req = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.AppManagement.Organization.postOrganizationOrganizationUpdate merchantShortId opCity apiTokenInfo personId req
