{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}


module API.Action.Dashboard.Operator.Registration 
( API.Types.ProviderPlatform.Operator.Registration.API,
handler )
where
import EulerHS.Prelude
import Servant
import Tools.Auth
import Kernel.Utils.Common
import qualified Domain.Action.Dashboard.Operator.Registration
import qualified Domain.Types.Merchant
import qualified Environment
import qualified Kernel.Types.Id
import qualified Kernel.Types.Beckn.Context
import qualified API.Types.ProviderPlatform.Operator.Registration



handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API.Types.ProviderPlatform.Operator.Registration.API)
handler merchantId city = postOperatorRegister merchantId city :<|> postRegistrationDashboardRegister merchantId city
postOperatorRegister :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> API.Types.ProviderPlatform.Operator.Registration.OperatorRegisterReq -> Environment.FlowHandler API.Types.ProviderPlatform.Operator.Registration.OperatorRegisterResp)
postOperatorRegister a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Operator.Registration.postOperatorRegister a3 a2 a1
postRegistrationDashboardRegister :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> API.Types.ProviderPlatform.Operator.Registration.CreateDashboardOperatorReq -> Environment.FlowHandler API.Types.ProviderPlatform.Operator.Registration.OperatorRegisterResp)
postRegistrationDashboardRegister a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Operator.Registration.postRegistrationDashboardRegister a3 a2 a1



