{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.DashboardAuth.Fleet.Onboarding
  ( API,
    handler,
  )
where

import qualified API.Types.ProviderPlatform.Fleet.Endpoints.OnboardingExtra
import qualified API.Types.ProviderPlatform.Fleet.Onboarding
import qualified API.Types.ProviderPlatform.Management.Endpoints.Account
import qualified Dashboard.Common
import qualified Domain.Action.Dashboard.Fleet.Onboarding
import qualified Domain.Types.Merchant
import qualified Domain.Types.VehicleCategory
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import qualified SharedLogic.Fleet
import Tools.Auth
import qualified Tools.Auth.DashboardRegistration
import Tools.Auth.DashboardUserAuth

type API = ("onboarding" :> (GetOnboardingDocumentConfigs :<|> GetOnboardingRegisterStatus :<|> GetOnboardingRegisterVehicleStatus :<|> PostOnboardingVerify :<|> GetOnboardingVehicleDocuments :<|> GetOnboardingGetReferralDetails))

type GetOnboardingDocumentConfigs =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_FLEET/ONBOARDING/GET_ONBOARDING_DOCUMENT_CONFIGS"
      :> API.Types.ProviderPlatform.Fleet.Onboarding.GetOnboardingDocumentConfigs
  )

type GetOnboardingRegisterStatus =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_FLEET/ONBOARDING/GET_ONBOARDING_REGISTER_STATUS"
      :> API.Types.ProviderPlatform.Fleet.Onboarding.GetOnboardingRegisterStatus
  )

type GetOnboardingRegisterVehicleStatus =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_FLEET/ONBOARDING/GET_ONBOARDING_REGISTER_VEHICLE_STATUS"
      :> API.Types.ProviderPlatform.Fleet.Onboarding.GetOnboardingRegisterVehicleStatus
  )

type PostOnboardingVerify =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_FLEET/ONBOARDING/POST_ONBOARDING_VERIFY"
      :> API.Types.ProviderPlatform.Fleet.Onboarding.PostOnboardingVerify
  )

type GetOnboardingVehicleDocuments =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_FLEET/ONBOARDING/GET_ONBOARDING_VEHICLE_DOCUMENTS"
      :> API.Types.ProviderPlatform.Fleet.Onboarding.GetOnboardingVehicleDocuments
  )

type GetOnboardingGetReferralDetails =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_FLEET/ONBOARDING/GET_ONBOARDING_GET_REFERRAL_DETAILS"
      :> API.Types.ProviderPlatform.Fleet.Onboarding.GetOnboardingGetReferralDetails
  )

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = getOnboardingDocumentConfigs merchantId city :<|> getOnboardingRegisterStatus merchantId city :<|> getOnboardingRegisterVehicleStatus merchantId city :<|> postOnboardingVerify merchantId city :<|> getOnboardingVehicleDocuments merchantId city :<|> getOnboardingGetReferralDetails merchantId city

getOnboardingDocumentConfigs :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (Kernel.Prelude.Bool) -> Kernel.Prelude.Maybe (Kernel.Prelude.Bool) -> Kernel.Prelude.Maybe (API.Types.ProviderPlatform.Fleet.Onboarding.Role) -> Kernel.Prelude.Maybe (API.Types.ProviderPlatform.Fleet.Endpoints.OnboardingExtra.DocumentOnboardingStage) -> Environment.FlowHandler API.Types.ProviderPlatform.Fleet.Onboarding.DocumentVerificationConfigList)
getOnboardingDocumentConfigs a7 a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ do
  fleetOwnerId <- SharedLogic.Fleet.getFleetOwnerId (Tools.Auth.DashboardUserAuth.dashboardRequestorId a5) Kernel.Prelude.Nothing
  Domain.Action.Dashboard.Fleet.Onboarding.getOnboardingDocumentConfigs a7 a6 fleetOwnerId a4 a3 a2 a1

getOnboardingRegisterStatus :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id Dashboard.Common.Driver) -> Kernel.Prelude.Maybe (Kernel.Prelude.Bool) -> Kernel.Prelude.Maybe (Domain.Types.VehicleCategory.VehicleCategory) -> Kernel.Prelude.Maybe (Kernel.Prelude.Bool) -> Kernel.Prelude.Maybe (Kernel.Prelude.Bool) -> Kernel.Prelude.Maybe (Dashboard.Common.DocsVerificationStatus) -> Kernel.Prelude.Maybe (Kernel.Prelude.Bool) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Environment.FlowHandler API.Types.ProviderPlatform.Fleet.Onboarding.StatusRes)
getOnboardingRegisterStatus a11 a10 a9 a8 a7 a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ do
  fleetOwnerId <- SharedLogic.Fleet.getFleetOwnerId (Tools.Auth.DashboardUserAuth.dashboardRequestorId a9) Kernel.Prelude.Nothing
  Domain.Action.Dashboard.Fleet.Onboarding.getOnboardingRegisterStatus a11 a10 fleetOwnerId a8 a7 a6 a5 a4 a3 a2 a1

getOnboardingRegisterVehicleStatus :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Bool) -> Environment.FlowHandler API.Types.ProviderPlatform.Fleet.Onboarding.RcVerifyStatusResp)
getOnboardingRegisterVehicleStatus a6 a5 _a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Fleet.Onboarding.getOnboardingRegisterVehicleStatus a6 a5 a3 a2 a1

postOnboardingVerify :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> API.Types.ProviderPlatform.Fleet.Onboarding.VerifyType -> API.Types.ProviderPlatform.Fleet.Onboarding.VerifyReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postOnboardingVerify a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ do
  adminApprovalRequired <- Tools.Auth.DashboardRegistration.adminApprovalRequiredForDriver a5.getShortId a1.driverId
  res <- Domain.Action.Dashboard.Fleet.Onboarding.postOnboardingVerify a5 a4 a2 (Tools.Auth.DashboardRegistration.requestorDashboardAccessType a3) (Kernel.Prelude.Just adminApprovalRequired) a1
  Kernel.Prelude.when res.enableFleetOwner $ Tools.Auth.DashboardRegistration.markDashboardPersonVerifiedOnOnboarding a1.driverId
  Kernel.Prelude.pure Kernel.Types.APISuccess.Success

getOnboardingVehicleDocuments :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Bool) -> Environment.FlowHandler API.Types.ProviderPlatform.Fleet.Onboarding.VehicleDocumentStatusRes)
getOnboardingVehicleDocuments a6 a5 _a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Fleet.Onboarding.getOnboardingVehicleDocuments a6 a5 a3 a2 a1

getOnboardingGetReferralDetails :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Text -> Environment.FlowHandler API.Types.ProviderPlatform.Fleet.Onboarding.ReferralInfoRes)
getOnboardingGetReferralDetails a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Fleet.Onboarding.getOnboardingGetReferralDetails a4 a3 (Tools.Auth.DashboardUserAuth.dashboardRequestorId a2) a1
