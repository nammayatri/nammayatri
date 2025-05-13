{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.Dashboard.Management.Customer
  ( API.Types.RiderPlatform.Management.Customer.API,
    handler,
  )
where

import qualified API.Types.RiderPlatform.Management.Customer
import qualified Dashboard.Common
import qualified Domain.Action.Dashboard.Customer as Domain.Action.Dashboard.Customer
import qualified Domain.Types.Merchant
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Tools.Auth

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API.Types.RiderPlatform.Management.Customer.API)
handler merchantId city = getCustomerList merchantId city :<|> deleteCustomerDelete merchantId city :<|> postCustomerBlock merchantId city :<|> postCustomerUnblock merchantId city :<|> getCustomerInfo merchantId city :<|> postCustomerCancellationDuesSync merchantId city :<|> getCustomerCancellationDuesDetails merchantId city :<|> postCustomerUpdateSafetyCenterBlocking merchantId city :<|> postCustomerPersonNumbers merchantId city :<|> postCustomerPersonId merchantId city

getCustomerList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id Dashboard.Common.Customer) -> Environment.FlowHandler API.Types.RiderPlatform.Management.Customer.CustomerListRes)
getCustomerList a8 a7 a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Customer.getCustomerList a8 a7 a6 a5 a4 a3 a2 a1

deleteCustomerDelete :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Dashboard.Common.Customer -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
deleteCustomerDelete a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Customer.deleteCustomerDelete a3 a2 a1

postCustomerBlock :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Dashboard.Common.Customer -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postCustomerBlock a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Customer.postCustomerBlock a4 a3 a2 a1

postCustomerUnblock :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Dashboard.Common.Customer -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postCustomerUnblock a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Customer.postCustomerUnblock a3 a2 a1

getCustomerInfo :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Dashboard.Common.Customer -> Environment.FlowHandler API.Types.RiderPlatform.Management.Customer.CustomerInfoRes)
getCustomerInfo a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Customer.getCustomerInfo a3 a2 a1

postCustomerCancellationDuesSync :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Dashboard.Common.Customer -> API.Types.RiderPlatform.Management.Customer.CustomerCancellationDuesSyncReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postCustomerCancellationDuesSync a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Customer.postCustomerCancellationDuesSync a4 a3 a2 a1

getCustomerCancellationDuesDetails :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Dashboard.Common.Customer -> Environment.FlowHandler API.Types.RiderPlatform.Management.Customer.CancellationDuesDetailsRes)
getCustomerCancellationDuesDetails a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Customer.getCustomerCancellationDuesDetails a3 a2 a1

postCustomerUpdateSafetyCenterBlocking :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Dashboard.Common.Customer -> API.Types.RiderPlatform.Management.Customer.UpdateSafetyCenterBlockingReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postCustomerUpdateSafetyCenterBlocking a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Customer.postCustomerUpdateSafetyCenterBlocking a4 a3 a2 a1

postCustomerPersonNumbers :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Dashboard.Common.PersonIdsReq -> Environment.FlowHandler [Dashboard.Common.PersonRes])
postCustomerPersonNumbers a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Customer.postCustomerPersonNumbers a3 a2 a1

postCustomerPersonId :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Dashboard.Common.PersonMobileNoReq -> Environment.FlowHandler [Dashboard.Common.PersonRes])
postCustomerPersonId a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Customer.postCustomerPersonId a3 a2 a1
