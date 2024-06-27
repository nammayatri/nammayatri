{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.Dashboard.Merchant
  ( API.Types.RiderPlatform.Merchant.API,
    handler,
  )
where

import qualified API.Types.RiderPlatform.Merchant
import qualified Dashboard.Common.Merchant
import qualified Domain.Action.Dashboard.Merchant as Domain.Action.Dashboard.Merchant
import qualified Domain.Types.Merchant
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Tools.Auth

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API.Types.RiderPlatform.Merchant.API)
handler merchantId city = postMerchantUpdate merchantId city :<|> postMerchantServiceConfigMapsUpdate merchantId city

postMerchantUpdate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> API.Types.RiderPlatform.Merchant.MerchantUpdateReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postMerchantUpdate a3 a2 a1 = withFlowHandlerAPI $ Domain.Action.Dashboard.Merchant.postMerchantUpdate a3 a2 a1

postMerchantServiceConfigMapsUpdate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Dashboard.Common.Merchant.MapsServiceConfigUpdateReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postMerchantServiceConfigMapsUpdate a3 a2 a1 = withFlowHandlerAPI $ Domain.Action.Dashboard.Merchant.postMerchantServiceConfigMapsUpdate a3 a2 a1
