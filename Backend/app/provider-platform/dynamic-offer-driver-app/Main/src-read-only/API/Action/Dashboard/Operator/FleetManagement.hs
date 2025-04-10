{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.Dashboard.Operator.FleetManagement
  ( API.Types.ProviderPlatform.Operator.FleetManagement.API,
    handler,
  )
where

import qualified API.Types.ProviderPlatform.Operator.FleetManagement
import qualified Domain.Action.Dashboard.Operator.FleetManagement as Domain.Action.Dashboard.Operator.FleetManagement
import qualified Domain.Types.Merchant
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Tools.Auth

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API.Types.ProviderPlatform.Operator.FleetManagement.API)
handler merchantId city = getFleetManagementFleets merchantId city

getFleetManagementFleets :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Maybe (Kernel.Prelude.Bool) -> Kernel.Prelude.Maybe (Kernel.Prelude.Bool) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Text -> Environment.FlowHandler [API.Types.ProviderPlatform.Operator.FleetManagement.FleetInfo])
getFleetManagementFleets a7 a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Operator.FleetManagement.getFleetManagementFleets a7 a6 a5 a4 a3 a2 a1
