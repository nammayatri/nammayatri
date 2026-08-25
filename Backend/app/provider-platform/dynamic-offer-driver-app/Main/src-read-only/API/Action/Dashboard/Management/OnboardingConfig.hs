{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.Dashboard.Management.OnboardingConfig
  ( API.Types.ProviderPlatform.Management.OnboardingConfig.API,
    handler,
  )
where

import qualified API.Types.ProviderPlatform.Management.OnboardingConfig
import qualified Domain.Action.Dashboard.Management.OnboardingConfig
import qualified Domain.Types.Merchant
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Tools.Auth

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API.Types.ProviderPlatform.Management.OnboardingConfig.API)
handler merchantId city = getOnboardingConfigGet merchantId city :<|> postOnboardingConfigClone merchantId city :<|> postOnboardingConfigApply merchantId city

getOnboardingConfigGet :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowHandler API.Types.ProviderPlatform.Management.OnboardingConfig.OnboardingConfigRes)
getOnboardingConfigGet a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.OnboardingConfig.getOnboardingConfigGet a2 a1

postOnboardingConfigClone :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> API.Types.ProviderPlatform.Management.OnboardingConfig.CloneConfigReq -> Environment.FlowHandler API.Types.ProviderPlatform.Management.OnboardingConfig.CloneConfigRes)
postOnboardingConfigClone a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.OnboardingConfig.postOnboardingConfigClone a3 a2 a1

postOnboardingConfigApply :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> API.Types.ProviderPlatform.Management.OnboardingConfig.ApplyConfigReq -> Environment.FlowHandler API.Types.ProviderPlatform.Management.OnboardingConfig.ApplyConfigRes)
postOnboardingConfigApply a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.OnboardingConfig.postOnboardingConfigApply a3 a2 a1
