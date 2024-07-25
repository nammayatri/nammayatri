{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.ProviderPlatform.DynamicOfferDriver.Management.Driver
  ( API,
    handler,
  )
where

import qualified API.Types.ProviderPlatform.Management.Driver
import qualified Dashboard.Common.Driver
import qualified Dashboard.ProviderPlatform.Driver
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified Domain.Types.Transaction
import qualified "lib-dashboard" Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import qualified ProviderPlatformClient.DynamicOfferDriver.Operations
import Servant
import qualified SharedLogic.Transaction
import Storage.Beam.CommonInstances ()
import Tools.Auth.Api
import Tools.Auth.Merchant

type API = ("driver" :> PostDriverClearFee)

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = postDriverClearFee merchantId city

type PostDriverClearFee = (ApiAuth 'DRIVER_OFFER_BPP_MANAGEMENT 'DRIVERS 'CLEAR_FEE :> API.Types.ProviderPlatform.Management.Driver.PostDriverClearFee)

postDriverClearFee :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Dashboard.ProviderPlatform.Driver.Driver -> API.Types.ProviderPlatform.Management.Driver.ClearDriverFeeReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postDriverClearFee merchantShortId opCity apiTokenInfo a1 req =
  withFlowHandlerAPI' $
    ( do
        checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
        transaction <- SharedLogic.Transaction.buildTransaction (Domain.Types.Transaction.DriverAPI Dashboard.Common.Driver.PostDriverClearFeeEndpoint) (Kernel.Prelude.Just DRIVER_OFFER_BPP_MANAGEMENT) (Kernel.Prelude.Just apiTokenInfo) Kernel.Prelude.Nothing Kernel.Prelude.Nothing (Kernel.Prelude.Just req)
        SharedLogic.Transaction.withTransactionStoring transaction $ (do ProviderPlatformClient.DynamicOfferDriver.Operations.callDriverOfferBPPOperations checkedMerchantId opCity (.driverDSL.postDriverClearFee) a1 req)
    )
