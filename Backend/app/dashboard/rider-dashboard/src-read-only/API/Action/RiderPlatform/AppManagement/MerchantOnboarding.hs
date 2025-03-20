{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.RiderPlatform.AppManagement.MerchantOnboarding
  ( API,
    handler,
  )
where

import qualified API.Types.Dashboard.AppManagement
import qualified "rider-app" API.Types.Dashboard.AppManagement.MerchantOnboarding
import qualified Domain.Action.RiderPlatform.AppManagement.MerchantOnboarding
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified "rider-app" Domain.Types.MerchantOnboarding
import qualified "lib-dashboard" Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Storage.Beam.CommonInstances ()
import Tools.Auth.Api

type API = (MerchantOnboardingInfo :<|> MerchantOnboardingStart :<|> MerchantOnboardingList)

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = merchantOnboardingInfo merchantId city :<|> merchantOnboardingStart merchantId city :<|> merchantOnboardingList merchantId city

type MerchantOnboardingInfo =
  ( ApiAuth
      ('APP_BACKEND_MANAGEMENT)
      ('DSL)
      (('RIDER_APP_MANAGEMENT) / ('API.Types.Dashboard.AppManagement.MERCHANT_ONBOARDING) / ('API.Types.Dashboard.AppManagement.MerchantOnboarding.MERCHANT_ONBOARDING_INFO))
      :> API.Types.Dashboard.AppManagement.MerchantOnboarding.MerchantOnboardingInfo
  )

type MerchantOnboardingStart =
  ( ApiAuth
      ('APP_BACKEND_MANAGEMENT)
      ('DSL)
      (('RIDER_APP_MANAGEMENT) / ('API.Types.Dashboard.AppManagement.MERCHANT_ONBOARDING) / ('API.Types.Dashboard.AppManagement.MerchantOnboarding.MERCHANT_ONBOARDING_START))
      :> API.Types.Dashboard.AppManagement.MerchantOnboarding.MerchantOnboardingStart
  )

type MerchantOnboardingList =
  ( ApiAuth
      ('APP_BACKEND_MANAGEMENT)
      ('DSL)
      (('RIDER_APP_MANAGEMENT) / ('API.Types.Dashboard.AppManagement.MERCHANT_ONBOARDING) / ('API.Types.Dashboard.AppManagement.MerchantOnboarding.MERCHANT_ONBOARDING_LIST))
      :> API.Types.Dashboard.AppManagement.MerchantOnboarding.MerchantOnboardingList
  )

merchantOnboardingInfo :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Domain.Types.MerchantOnboarding.OnboardingType -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Environment.FlowHandler Domain.Types.MerchantOnboarding.MerchantOnboardingAPI)
merchantOnboardingInfo merchantShortId opCity apiTokenInfo onboardingType requestorId = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.AppManagement.MerchantOnboarding.merchantOnboardingInfo merchantShortId opCity apiTokenInfo onboardingType requestorId

merchantOnboardingStart :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Domain.Types.MerchantOnboarding.OnboardingType -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Environment.FlowHandler Domain.Types.MerchantOnboarding.MerchantOnboardingAPI)
merchantOnboardingStart merchantShortId opCity apiTokenInfo onboardingType requestorId = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.AppManagement.MerchantOnboarding.merchantOnboardingStart merchantShortId opCity apiTokenInfo onboardingType requestorId

merchantOnboardingList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Environment.FlowHandler [Domain.Types.MerchantOnboarding.MerchantOnboarding])
merchantOnboardingList merchantShortId opCity apiTokenInfo requestorId = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.AppManagement.MerchantOnboarding.merchantOnboardingList merchantShortId opCity apiTokenInfo requestorId
