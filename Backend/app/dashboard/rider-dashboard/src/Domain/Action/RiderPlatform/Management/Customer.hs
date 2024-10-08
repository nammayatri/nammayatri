{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Action.RiderPlatform.Management.Customer (getCustomerList, deleteCustomerDelete) where

import qualified API.Types.RiderPlatform.Management
import qualified API.Types.RiderPlatform.Management.Customer
import qualified Dashboard.Common
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified Domain.Types.Transaction
import qualified "lib-dashboard" Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import qualified RiderPlatformClient.RiderApp.Operations
import qualified SharedLogic.Transaction
import Storage.Beam.CommonInstances ()
import Tools.Auth.Api
import Tools.Auth.Merchant

getCustomerList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id Dashboard.Common.Customer) -> Environment.Flow API.Types.RiderPlatform.Management.Customer.CustomerListRes)
getCustomerList merchantShortId opCity apiTokenInfo limit offset enabled blocked phone personId = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  RiderPlatformClient.RiderApp.Operations.callRiderAppOperations checkedMerchantId opCity (.customerDSL.getCustomerList) limit offset enabled blocked phone personId

deleteCustomerDelete ::
  Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.Context.City ->
  ApiTokenInfo ->
  Kernel.Types.Id.Id Dashboard.Common.Customer ->
  Environment.Flow Kernel.Types.APISuccess.APISuccess
deleteCustomerDelete merchantShortId opCity apiTokenInfo customerId = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.RiderManagementAPI (API.Types.RiderPlatform.Management.CustomerAPI API.Types.RiderPlatform.Management.Customer.DeleteCustomerDeleteEndpoint)) (Kernel.Prelude.Just APP_BACKEND_MANAGEMENT) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing Kernel.Prelude.Nothing SharedLogic.Transaction.emptyRequest
  SharedLogic.Transaction.withTransactionStoring transaction $
    RiderPlatformClient.RiderApp.Operations.callRiderAppOperations checkedMerchantId opCity (.customerDSL.deleteCustomerDelete) customerId
