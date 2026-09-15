{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | Hand-written handlers for direct-dashboard routes whose request needs more
-- than the verified operator's id or name: fleet-owner resolution, fleet-owner
-- verification, dashboard-database writes after the call, and similar.
--
-- provider-dashboard did this work in its own hand-written
-- @Domain.Action.ProviderPlatform.*@ layer before forwarding the call. The
-- generated @API.Action.DashboardAuth@ handler calls these functions instead of
-- the domain handler for every endpoint marked @appServerHandler: custom@ in
-- the spec, so this logic lives here and is never overwritten by the generator.
module Domain.Action.DashboardAuth.Fleet.Onboarding
  ( getOnboardingDocumentConfigs,
    getOnboardingRegisterStatus,
    postOnboardingVerify,
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

getOnboardingDocumentConfigs :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (Kernel.Prelude.Bool) -> Kernel.Prelude.Maybe (Kernel.Prelude.Bool) -> Kernel.Prelude.Maybe (API.Types.ProviderPlatform.Fleet.Onboarding.Role) -> Kernel.Prelude.Maybe (API.Types.ProviderPlatform.Fleet.Endpoints.OnboardingExtra.DocumentOnboardingStage) -> Environment.Flow API.Types.ProviderPlatform.Fleet.Onboarding.DocumentVerificationConfigList)
getOnboardingDocumentConfigs a7 a6 a5 a4 a3 a2 a1 = do
  fleetOwnerId <- SharedLogic.Fleet.getFleetOwnerId (Tools.Auth.DashboardUserAuth.dashboardRequestorId a5) Kernel.Prelude.Nothing
  Domain.Action.Dashboard.Fleet.Onboarding.getOnboardingDocumentConfigs a7 a6 fleetOwnerId a4 a3 a2 a1

getOnboardingRegisterStatus :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id Dashboard.Common.Driver) -> Kernel.Prelude.Maybe (Kernel.Prelude.Bool) -> Kernel.Prelude.Maybe (Domain.Types.VehicleCategory.VehicleCategory) -> Kernel.Prelude.Maybe (Kernel.Prelude.Bool) -> Kernel.Prelude.Maybe (Kernel.Prelude.Bool) -> Kernel.Prelude.Maybe (Dashboard.Common.DocsVerificationStatus) -> Kernel.Prelude.Maybe (Kernel.Prelude.Bool) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Environment.Flow API.Types.ProviderPlatform.Fleet.Onboarding.StatusRes)
getOnboardingRegisterStatus a11 a10 a9 a8 a7 a6 a5 a4 a3 a2 a1 = do
  fleetOwnerId <- SharedLogic.Fleet.getFleetOwnerId (Tools.Auth.DashboardUserAuth.dashboardRequestorId a9) Kernel.Prelude.Nothing
  Domain.Action.Dashboard.Fleet.Onboarding.getOnboardingRegisterStatus a11 a10 fleetOwnerId a8 a7 a6 a5 a4 a3 a2 a1

postOnboardingVerify :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> API.Types.ProviderPlatform.Fleet.Onboarding.VerifyType -> API.Types.ProviderPlatform.Fleet.Onboarding.VerifyReq -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postOnboardingVerify a5 a4 a3 a2 a1 = do
  adminApprovalRequired <- Tools.Auth.DashboardRegistration.adminApprovalRequiredForDriver a5.getShortId a1.driverId
  res <- Domain.Action.Dashboard.Fleet.Onboarding.postOnboardingVerify a5 a4 a2 (Tools.Auth.DashboardRegistration.requestorDashboardAccessType a3) (Kernel.Prelude.Just adminApprovalRequired) a1
  Kernel.Prelude.when res.enableFleetOwner $ Tools.Auth.DashboardRegistration.markDashboardPersonVerifiedOnOnboarding a1.driverId
  Kernel.Prelude.pure Kernel.Types.APISuccess.Success
