{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.Dashboard.AppManagement.Organization
  ( API.Types.Dashboard.AppManagement.Organization.API,
    handler,
  )
where

import qualified API.Types.Dashboard.AppManagement.Organization
import qualified Domain.Action.Dashboard.AppManagement.Organization
import qualified Domain.Types.Merchant
import qualified "this" Domain.Types.Organization
import qualified "this" Domain.Types.Person
import qualified "this" Domain.Types.Student
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Tools.Auth

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API.Types.Dashboard.AppManagement.Organization.API)
handler merchantId city = getOrganizationGetOrganizationId merchantId city :<|> getOrganizationStudentOrganization merchantId city :<|> postOrganizationStudentVerify merchantId city :<|> postOrganizationOrganizationUpdate merchantId city

getOrganizationGetOrganizationId :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Domain.Types.Person.Person -> Environment.FlowHandler (Kernel.Types.Id.Id Domain.Types.Organization.Organization))
getOrganizationGetOrganizationId a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.Organization.getOrganizationGetOrganizationId a3 a2 a1

getOrganizationStudentOrganization :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Domain.Types.Organization.Organization -> Kernel.Prelude.Maybe (Domain.Types.Student.VerificationStatus) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Environment.FlowHandler API.Types.Dashboard.AppManagement.Organization.StudentsListResp)
getOrganizationStudentOrganization a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.Organization.getOrganizationStudentOrganization a6 a5 a4 a3 a2 a1

postOrganizationStudentVerify :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> API.Types.Dashboard.AppManagement.Organization.VerifyStudentsReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postOrganizationStudentVerify a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.Organization.postOrganizationStudentVerify a3 a2 a1

postOrganizationOrganizationUpdate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Domain.Types.Person.Person -> API.Types.Dashboard.AppManagement.Organization.OrganizationUpdateReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postOrganizationOrganizationUpdate a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.Organization.postOrganizationOrganizationUpdate a4 a3 a2 a1
