{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.ProviderPlatform.Management.OnboardingConfig
  ( API,
    handler,
  )
where

import qualified API.Types.ProviderPlatform.Management
import qualified API.Types.ProviderPlatform.Management.OnboardingConfig
import qualified Domain.Action.ProviderPlatform.Management.OnboardingConfig
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified "lib-dashboard" Environment
import EulerHS.Prelude hiding (sortOn)
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common hiding (INFO)
import Servant
import Storage.Beam.CommonInstances ()
import Tools.Auth.Api

type API = ("onboardingConfig" :> (GetOnboardingConfigGet :<|> PostOnboardingConfigClone :<|> PostOnboardingConfigApply))

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = getOnboardingConfigGet merchantId city :<|> postOnboardingConfigClone merchantId city :<|> postOnboardingConfigApply merchantId city

type GetOnboardingConfigGet =
  ( ApiAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      ('DSL)
      (('PROVIDER_MANAGEMENT) / ('API.Types.ProviderPlatform.Management.ONBOARDING_CONFIG) / ('API.Types.ProviderPlatform.Management.OnboardingConfig.GET_ONBOARDING_CONFIG_GET))
      :> API.Types.ProviderPlatform.Management.OnboardingConfig.GetOnboardingConfigGet
  )

type PostOnboardingConfigClone =
  ( ApiAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      ('DSL)
      (('PROVIDER_MANAGEMENT) / ('API.Types.ProviderPlatform.Management.ONBOARDING_CONFIG) / ('API.Types.ProviderPlatform.Management.OnboardingConfig.POST_ONBOARDING_CONFIG_CLONE))
      :> API.Types.ProviderPlatform.Management.OnboardingConfig.PostOnboardingConfigClone
  )

type PostOnboardingConfigApply =
  ( ApiAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      ('DSL)
      (('PROVIDER_MANAGEMENT) / ('API.Types.ProviderPlatform.Management.ONBOARDING_CONFIG) / ('API.Types.ProviderPlatform.Management.OnboardingConfig.POST_ONBOARDING_CONFIG_APPLY))
      :> API.Types.ProviderPlatform.Management.OnboardingConfig.PostOnboardingConfigApply
  )

getOnboardingConfigGet :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Environment.FlowHandler API.Types.ProviderPlatform.Management.OnboardingConfig.OnboardingConfigRes)
getOnboardingConfigGet merchantShortId opCity apiTokenInfo = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.OnboardingConfig.getOnboardingConfigGet merchantShortId opCity apiTokenInfo

postOnboardingConfigClone :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> API.Types.ProviderPlatform.Management.OnboardingConfig.CloneConfigReq -> Environment.FlowHandler API.Types.ProviderPlatform.Management.OnboardingConfig.CloneConfigRes)
postOnboardingConfigClone merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.OnboardingConfig.postOnboardingConfigClone merchantShortId opCity apiTokenInfo req

postOnboardingConfigApply :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> API.Types.ProviderPlatform.Management.OnboardingConfig.ApplyConfigReq -> Environment.FlowHandler API.Types.ProviderPlatform.Management.OnboardingConfig.ApplyConfigRes)
postOnboardingConfigApply merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.OnboardingConfig.postOnboardingConfigApply merchantShortId opCity apiTokenInfo req
