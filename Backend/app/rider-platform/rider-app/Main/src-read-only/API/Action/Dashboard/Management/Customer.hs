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
handler merchantId city = getCustomerList merchantId city :<|> deleteCustomerDelete merchantId city :<|> postCustomerBlock merchantId city

getCustomerList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id Dashboard.Common.Customer) -> Environment.FlowHandler API.Types.RiderPlatform.Management.Customer.CustomerListRes)
getCustomerList a8 a7 a6 a5 a4 a3 a2 a1 = withFlowHandlerAPI $ Domain.Action.Dashboard.Customer.getCustomerList a8 a7 a6 a5 a4 a3 a2 a1

deleteCustomerDelete :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Dashboard.Common.Customer -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
deleteCustomerDelete a3 a2 a1 = withFlowHandlerAPI $ Domain.Action.Dashboard.Customer.deleteCustomerDelete a3 a2 a1

postCustomerBlock :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Dashboard.Common.Customer -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postCustomerBlock a3 a2 a1 = withFlowHandlerAPI $ Domain.Action.Dashboard.Customer.postCustomerBlock a3 a2 a1
