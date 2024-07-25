{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.Dashboard.Management.Driver
  ( API.Types.ProviderPlatform.Management.Driver.API,
    handler,
  )
where

import qualified API.Types.ProviderPlatform.Management.Driver
import qualified Dashboard.ProviderPlatform.Driver
import qualified Domain.Action.Dashboard.Driver as Domain.Action.Dashboard.Driver
import qualified Domain.Types.Merchant
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Tools.Auth

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API.Types.ProviderPlatform.Management.Driver.API)
handler merchantId city = postDriverClearFee merchantId city

postDriverClearFee :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id Dashboard.ProviderPlatform.Driver.Driver -> API.Types.ProviderPlatform.Management.Driver.ClearDriverFeeReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postDriverClearFee a4 a3 a2 a1 = withFlowHandlerAPI $ Domain.Action.Dashboard.Driver.postDriverClearFee a4 a3 a2 a1
