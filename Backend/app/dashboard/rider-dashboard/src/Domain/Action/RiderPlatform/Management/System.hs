{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Action.RiderPlatform.Management.System (postSystemRunQuery) where

import qualified API.Types.RiderPlatform.Management
import qualified API.Types.RiderPlatform.Management.System
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

postSystemRunQuery :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> API.Types.RiderPlatform.Management.System.QueryData -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postSystemRunQuery merchantShortId opCity apiTokenInfo req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.RiderManagementAPI (API.Types.RiderPlatform.Management.SystemAPI API.Types.RiderPlatform.Management.System.PostSystemRunQueryEndpoint)) (Kernel.Prelude.Just APP_BACKEND_MANAGEMENT) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing Kernel.Prelude.Nothing (Kernel.Prelude.Just req)
  SharedLogic.Transaction.withTransactionStoring transaction $ (do RiderPlatformClient.RiderApp.Operations.callRiderAppOperations checkedMerchantId opCity (.systemDSL.postSystemRunQuery) req)
