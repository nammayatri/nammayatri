{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.Dashboard.Operator.Fleet
  ( API.Types.ProviderPlatform.Operator.Fleet.API,
    handler,
  )
where

import qualified API.Types.ProviderPlatform.Operator.Fleet
import qualified Domain.Action.Dashboard.Operator.Fleet as Domain.Action.Dashboard.Operator.Fleet
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

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API.Types.ProviderPlatform.Operator.Fleet.API)
handler merchantId city = postFleetOperatorFleetLink merchantId city :<|> postFleetOperatorFleetUnlink merchantId city

postFleetOperatorFleetLink :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> Kernel.Prelude.Text -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postFleetOperatorFleetLink a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Operator.Fleet.postFleetOperatorFleetLink a4 a3 a2 a1

postFleetOperatorFleetUnlink :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> Kernel.Prelude.Text -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postFleetOperatorFleetUnlink a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Operator.Fleet.postFleetOperatorFleetUnlink a4 a3 a2 a1
