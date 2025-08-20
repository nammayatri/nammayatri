{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.Dashboard.Management.System
  ( API.Types.RiderPlatform.Management.System.API,
    handler,
  )
where

import qualified API.Types.RiderPlatform.Management.System
import qualified Domain.Action.Dashboard.System
import qualified Domain.Types.Merchant
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Tools.Auth

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API.Types.RiderPlatform.Management.System.API)
handler merchantId city = postSystemRunQuery merchantId city

postSystemRunQuery :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> API.Types.RiderPlatform.Management.System.QueryData -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postSystemRunQuery a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.System.postSystemRunQuery a3 a2 a1
