{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}


module API.Action.Dashboard.AppManagement.PassOrganization
( API.Types.Dashboard.AppManagement.PassOrganization.API,
handler )
where
import EulerHS.Prelude
import Servant
import Tools.Auth
import Kernel.Utils.Common
import qualified Domain.Action.Dashboard.AppManagement.PassOrganization
import qualified "this" Domain.Types.Person
import qualified Kernel.Prelude
import qualified Domain.Types.Merchant
import qualified Environment
import qualified Kernel.Types.Id
import qualified Kernel.Types.Beckn.Context
import qualified "this" Domain.Types.PassOrganization
import qualified "this" Domain.Types.PassDetails
import qualified API.Types.Dashboard.AppManagement.PassOrganization
import qualified Kernel.Types.APISuccess
import qualified "this" Domain.Types.PassType

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API.Types.Dashboard.AppManagement.PassOrganization.API)
handler merchantId city = getPassOrganizationGetPassOrganizationId merchantId city :<|> getPassOrganizationPassDetails merchantId city :<|> postPassOrganizationPassDetailsVerify merchantId city :<|> postPassOrganizationUpdate merchantId city :<|> getPassOrganizationGetOrganizations merchantId city

getPassOrganizationGetPassOrganizationId :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Domain.Types.Person.Person -> Environment.FlowHandler (Kernel.Types.Id.Id Domain.Types.PassOrganization.PassOrganization))
getPassOrganizationGetPassOrganizationId a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.PassOrganization.getPassOrganizationGetPassOrganizationId a3 a2 a1

getPassOrganizationPassDetails :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Domain.Types.PassOrganization.PassOrganization -> Kernel.Prelude.Maybe (Domain.Types.PassDetails.VerificationStatus) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Environment.FlowHandler API.Types.Dashboard.AppManagement.PassOrganization.PassDetailsListResp)
getPassOrganizationPassDetails a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.PassOrganization.getPassOrganizationPassDetails a6 a5 a4 a3 a2 a1

postPassOrganizationPassDetailsVerify :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> API.Types.Dashboard.AppManagement.PassOrganization.VerifyPassDetailsReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postPassOrganizationPassDetailsVerify a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.PassOrganization.postPassOrganizationPassDetailsVerify a3 a2 a1

postPassOrganizationUpdate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Domain.Types.Person.Person -> API.Types.Dashboard.AppManagement.PassOrganization.PassOrganizationUpdateReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postPassOrganizationUpdate a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.PassOrganization.postPassOrganizationUpdate a4 a3 a2 a1

getPassOrganizationGetOrganizations :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Domain.Types.PassType.PassEnum -> Environment.FlowHandler [API.Types.Dashboard.AppManagement.PassOrganization.GetOrganizationResp])
getPassOrganizationGetOrganizations a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.PassOrganization.getPassOrganizationGetOrganizations a3 a2 a1

