module Domain.Action.ProviderPlatform.Fleet.Onboarding (getOnboardingDocumentConfigs, getOnboardingRegisterStatus, postOnboardingVerify, getOnboardingGetReferralDetails) where

import qualified API.Client.ProviderPlatform.Fleet as Client
import qualified API.Types.ProviderPlatform.Fleet.Onboarding
import qualified Dashboard.Common
import Domain.Action.ProviderPlatform.Fleet.Driver (getFleetOwnerId)
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified Domain.Types.VehicleCategory
import qualified "lib-dashboard" Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Storage.Beam.CommonInstances ()
import Tools.Auth.Api
import Tools.Auth.Merchant

getOnboardingDocumentConfigs :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Prelude.Maybe API.Types.ProviderPlatform.Fleet.Onboarding.Role -> Environment.Flow API.Types.ProviderPlatform.Fleet.Onboarding.DocumentVerificationConfigList)
getOnboardingDocumentConfigs merchantShortId opCity apiTokenInfo makeSelfieAadhaarPanMandatory onlyVehicle role = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  fleetOwnerId <- getFleetOwnerId apiTokenInfo.personId.getId Nothing
  Client.callFleetAPI checkedMerchantId opCity (.onboardingDSL.getOnboardingDocumentConfigs) fleetOwnerId makeSelfieAadhaarPanMandatory onlyVehicle role

getOnboardingRegisterStatus :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id Dashboard.Common.Driver) -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Prelude.Maybe Domain.Types.VehicleCategory.VehicleCategory -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Environment.Flow API.Types.ProviderPlatform.Fleet.Onboarding.StatusRes)
getOnboardingRegisterStatus merchantShortId opCity apiTokenInfo driverId makeSelfieAadhaarPanMandatory onboardingVehicleCategory providePrefillDetails = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  fleetOwnerId <- getFleetOwnerId apiTokenInfo.personId.getId Nothing
  Client.callFleetAPI checkedMerchantId opCity (.onboardingDSL.getOnboardingRegisterStatus) fleetOwnerId driverId makeSelfieAadhaarPanMandatory onboardingVehicleCategory providePrefillDetails

postOnboardingVerify :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> API.Types.ProviderPlatform.Fleet.Onboarding.VerifyType -> API.Types.ProviderPlatform.Fleet.Onboarding.VerifyReq -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postOnboardingVerify merchantShortId opCity apiTokenInfo verifyType req = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  Client.callFleetAPI checkedMerchantId opCity (.onboardingDSL.postOnboardingVerify) verifyType apiTokenInfo.personId.getId req

getOnboardingGetReferralDetails :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Text -> Environment.Flow API.Types.ProviderPlatform.Fleet.Onboarding.ReferralInfoRes)
getOnboardingGetReferralDetails merchantShortId opCity apiTokenInfo referralCode = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  let requestorId = apiTokenInfo.personId.getId
  Client.callFleetAPI checkedMerchantId opCity (.onboardingDSL.getOnboardingGetReferralDetails) requestorId referralCode
