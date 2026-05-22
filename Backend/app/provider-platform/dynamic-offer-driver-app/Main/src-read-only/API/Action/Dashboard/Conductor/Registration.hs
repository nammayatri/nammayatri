{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.Dashboard.Conductor.Registration
  ( API.Types.ProviderPlatform.Conductor.Registration.API,
    handler,
  )
where

import qualified API.Types.ProviderPlatform.Conductor.Registration
import qualified Domain.Action.Dashboard.Conductor.Registration
import qualified Domain.Types.Merchant
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Tools.Auth

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API.Types.ProviderPlatform.Conductor.Registration.API)
handler merchantId city = postConductorRegister merchantId city :<|> postConductorBulkRegister merchantId city

postConductorRegister :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> API.Types.ProviderPlatform.Conductor.Registration.ConductorRegisterReq -> Environment.FlowHandler API.Types.ProviderPlatform.Conductor.Registration.ConductorRegisterResp)
postConductorRegister a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Conductor.Registration.postConductorRegister a3 a2 a1

postConductorBulkRegister :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> API.Types.ProviderPlatform.Conductor.Registration.ConductorBulkRegisterReq -> Environment.FlowHandler API.Types.ProviderPlatform.Conductor.Registration.ConductorBulkRegisterResp)
postConductorBulkRegister a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Conductor.Registration.postConductorBulkRegister a3 a2 a1
