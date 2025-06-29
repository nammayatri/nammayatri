{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.ProviderPlatform.Fleet.Onboarding
  ( API,
    handler,
  )
where

import qualified API.Types.ProviderPlatform.Fleet
import qualified API.Types.ProviderPlatform.Fleet.Onboarding
import qualified Dashboard.Common
import qualified Domain.Action.ProviderPlatform.Fleet.Onboarding
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified Domain.Types.VehicleCategory
import qualified "lib-dashboard" Environment
import EulerHS.Prelude hiding (sortOn)
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common hiding (INFO)
import Servant
import Storage.Beam.CommonInstances ()
import Tools.Auth.Api

type API = ("onboarding" :> (GetOnboardingDocumentConfigs :<|> GetOnboardingRegisterStatus :<|> PostOnboardingVerify :<|> GetOnboardingGetReferralDetails))

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = getOnboardingDocumentConfigs merchantId city :<|> getOnboardingRegisterStatus merchantId city :<|> postOnboardingVerify merchantId city :<|> getOnboardingGetReferralDetails merchantId city

type GetOnboardingDocumentConfigs =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_FLEET / 'API.Types.ProviderPlatform.Fleet.ONBOARDING / 'API.Types.ProviderPlatform.Fleet.Onboarding.GET_ONBOARDING_DOCUMENT_CONFIGS)
      :> API.Types.ProviderPlatform.Fleet.Onboarding.GetOnboardingDocumentConfigs
  )

type GetOnboardingRegisterStatus =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_FLEET / 'API.Types.ProviderPlatform.Fleet.ONBOARDING / 'API.Types.ProviderPlatform.Fleet.Onboarding.GET_ONBOARDING_REGISTER_STATUS)
      :> API.Types.ProviderPlatform.Fleet.Onboarding.GetOnboardingRegisterStatus
  )

type PostOnboardingVerify =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_FLEET / 'API.Types.ProviderPlatform.Fleet.ONBOARDING / 'API.Types.ProviderPlatform.Fleet.Onboarding.POST_ONBOARDING_VERIFY)
      :> API.Types.ProviderPlatform.Fleet.Onboarding.PostOnboardingVerify
  )

type GetOnboardingGetReferralDetails =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_FLEET / 'API.Types.ProviderPlatform.Fleet.ONBOARDING / 'API.Types.ProviderPlatform.Fleet.Onboarding.GET_ONBOARDING_GET_REFERRAL_DETAILS)
      :> API.Types.ProviderPlatform.Fleet.Onboarding.GetOnboardingGetReferralDetails
  )

getOnboardingDocumentConfigs :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Prelude.Maybe API.Types.ProviderPlatform.Fleet.Onboarding.Role -> Environment.FlowHandler API.Types.ProviderPlatform.Fleet.Onboarding.DocumentVerificationConfigList)
getOnboardingDocumentConfigs merchantShortId opCity apiTokenInfo makeSelfieAadhaarPanMandatory onlyVehicle role = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Fleet.Onboarding.getOnboardingDocumentConfigs merchantShortId opCity apiTokenInfo makeSelfieAadhaarPanMandatory onlyVehicle role

getOnboardingRegisterStatus :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id Dashboard.Common.Driver) -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Prelude.Maybe Domain.Types.VehicleCategory.VehicleCategory -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Environment.FlowHandler API.Types.ProviderPlatform.Fleet.Onboarding.StatusRes)
getOnboardingRegisterStatus merchantShortId opCity apiTokenInfo driverId makeSelfieAadhaarPanMandatory onboardingVehicleCategory providePrefillDetails onlyMandatoryDocs = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Fleet.Onboarding.getOnboardingRegisterStatus merchantShortId opCity apiTokenInfo driverId makeSelfieAadhaarPanMandatory onboardingVehicleCategory providePrefillDetails onlyMandatoryDocs

postOnboardingVerify :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> API.Types.ProviderPlatform.Fleet.Onboarding.VerifyType -> API.Types.ProviderPlatform.Fleet.Onboarding.VerifyReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postOnboardingVerify merchantShortId opCity apiTokenInfo verifyType req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Fleet.Onboarding.postOnboardingVerify merchantShortId opCity apiTokenInfo verifyType req

getOnboardingGetReferralDetails :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Text -> Environment.FlowHandler API.Types.ProviderPlatform.Fleet.Onboarding.ReferralInfoRes)
getOnboardingGetReferralDetails merchantShortId opCity apiTokenInfo referralCode = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Fleet.Onboarding.getOnboardingGetReferralDetails merchantShortId opCity apiTokenInfo referralCode
