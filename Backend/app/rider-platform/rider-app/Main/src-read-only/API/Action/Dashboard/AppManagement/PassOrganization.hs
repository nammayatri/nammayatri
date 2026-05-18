{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.Dashboard.AppManagement.PassOrganization
  ( API.Types.Dashboard.AppManagement.PassOrganization.API,
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

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API.Types.Dashboard.AppManagement.PassOrganization.API)
handler merchantId city = getPassOrganizationGetPassOrganization merchantId city :<|> getPassOrganizationPassDetails merchantId city :<|> postPassOrganizationPassDetailsVerify merchantId city :<|> postPassOrganizationUpdate merchantId city :<|> getPassOrganizationGetOrganizations merchantId city :<|> getPassOrganizationPassDetailsDocument merchantId city

getPassOrganizationGetPassOrganization :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Domain.Types.Person.Person -> Environment.FlowHandler API.Types.Dashboard.AppManagement.PassOrganization.GetOrganizationResp)
getPassOrganizationGetPassOrganization a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.PassOrganization.getPassOrganizationGetPassOrganization a3 a2 a1

getPassOrganizationPassDetails :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.PassOrganization.PassOrganization) -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Environment.FlowHandler API.Types.Dashboard.AppManagement.PassOrganization.PassDetailsListResp)
getPassOrganizationPassDetails a7 a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.PassOrganization.getPassOrganizationPassDetails a7 a6 a5 a4 a3 a2 a1

postPassOrganizationPassDetailsVerify :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> API.Types.Dashboard.AppManagement.PassOrganization.VerifyPassDetailsReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postPassOrganizationPassDetailsVerify a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.PassOrganization.postPassOrganizationPassDetailsVerify a3 a2 a1

postPassOrganizationUpdate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Domain.Types.Person.Person -> API.Types.Dashboard.AppManagement.PassOrganization.PassOrganizationUpdateReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postPassOrganizationUpdate a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.PassOrganization.postPassOrganizationUpdate a4 a3 a2 a1

getPassOrganizationGetOrganizations :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> Environment.FlowHandler [API.Types.Dashboard.AppManagement.PassOrganization.GetOrganizationResp])
getPassOrganizationGetOrganizations a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.PassOrganization.getPassOrganizationGetOrganizations a3 a2 a1

getPassOrganizationPassDetailsDocument :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id IssueManagement.Domain.Types.MediaFile.MediaFile -> Environment.FlowHandler Kernel.Prelude.Text)
getPassOrganizationPassDetailsDocument a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.PassOrganization.getPassOrganizationPassDetailsDocument a3 a2 a1
