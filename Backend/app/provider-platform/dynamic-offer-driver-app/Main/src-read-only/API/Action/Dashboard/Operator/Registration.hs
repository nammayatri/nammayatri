{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.Dashboard.Operator.Registration
  ( API.Types.ProviderPlatform.Operator.Registration.API,
    handler,
  )
where

import qualified API.Types.ProviderPlatform.Operator.Registration
import qualified Domain.Action.Dashboard.Operator.Registration
import qualified Domain.Types.Merchant
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Tools.Auth

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API.Types.ProviderPlatform.Operator.Registration.API)
handler merchantId city = postOperatorRegister merchantId city

postOperatorRegister :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> API.Types.ProviderPlatform.Operator.Registration.OperatorRegisterReq -> Environment.FlowHandler API.Types.ProviderPlatform.Operator.Registration.OperatorRegisterResp)
postOperatorRegister a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Operator.Registration.postOperatorRegister a3 a2 a1
