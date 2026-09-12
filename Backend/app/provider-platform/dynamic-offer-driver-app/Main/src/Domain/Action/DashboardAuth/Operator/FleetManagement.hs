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
module Domain.Action.DashboardAuth.Operator.FleetManagement
  ( postFleetManagementFleetCreate,
    postFleetManagementFleetRegister,
    postFleetManagementFleetLinkSendOtp,
  )
where

import qualified API.Types.ProviderPlatform.Fleet.Endpoints.RegistrationV2
import qualified API.Types.ProviderPlatform.Operator.FleetManagement
import qualified Dashboard.ProviderPlatform.Fleet.RegistrationV2
import qualified Domain.Action.Dashboard.Operator.FleetManagement
import qualified Domain.Types.Merchant
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import qualified Kernel.Utils.Validation
import Servant
import Tools.Auth
import qualified Tools.Auth.DashboardRegistration
import Tools.Auth.DashboardUserAuth

postFleetManagementFleetCreate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> API.Types.ProviderPlatform.Fleet.Endpoints.RegistrationV2.FleetOwnerLoginReqV2 -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postFleetManagementFleetCreate a4 a3 a2 a1 = do
  mbDashboardPersonId <- Tools.Auth.DashboardRegistration.dashboardPersonIdByMobile a1.mobileNumber a1.mobileCountryCode
  enabled <- Tools.Auth.DashboardRegistration.fleetOnboardingEnabled a4.getShortId a3
  res <- Domain.Action.Dashboard.Operator.FleetManagement.postFleetManagementFleetCreate a4 a3 (Kernel.Prelude.Just enabled) mbDashboardPersonId (Tools.Auth.DashboardUserAuth.dashboardRequestorId a2) a1
  Kernel.Prelude.when (Kernel.Prelude.isNothing mbDashboardPersonId) $
    Tools.Auth.DashboardRegistration.createFleetOwnerDashboardPerson a4.getShortId a3 a1.mobileNumber a1.mobileCountryCode res.personId.getId
  Kernel.Prelude.pure Kernel.Types.APISuccess.Success

postFleetManagementFleetRegister :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> API.Types.ProviderPlatform.Fleet.Endpoints.RegistrationV2.FleetOwnerRegisterReqV2 -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postFleetManagementFleetRegister a4 a3 a2 a1 = do
  reg <- Tools.Auth.DashboardUserAuth.beginFleetOwnerRegistration a2 a3 a1.email (Kernel.Types.Id.getId Kernel.Prelude.<$> a1.personId)
  Kernel.Utils.Validation.runRequestValidation (if reg.strongNameCheck then Dashboard.ProviderPlatform.Fleet.RegistrationV2.validateRegisterReqV2 else Dashboard.ProviderPlatform.Fleet.RegistrationV2.validateRegisterReqWithLooseCheck) a1
  let req = a1 {API.Types.ProviderPlatform.Fleet.Endpoints.RegistrationV2.adminApprovalRequired = reg.adminApprovalRequired, API.Types.ProviderPlatform.Fleet.Endpoints.RegistrationV2.email = reg.normalizedEmail} :: API.Types.ProviderPlatform.Fleet.Endpoints.RegistrationV2.FleetOwnerRegisterReqV2
  res <- Domain.Action.Dashboard.Operator.FleetManagement.postFleetManagementFleetRegister a4 a3 reg.requestorId req
  Tools.Auth.DashboardUserAuth.completeFleetOwnerRegistration reg.fleetOwnerId reg.normalizedEmail a1.firstName a1.lastName (a1.fleetType Kernel.Prelude.== Kernel.Prelude.Just API.Types.ProviderPlatform.Fleet.Endpoints.RegistrationV2.RENTAL_FLEET) res.enabled
  Kernel.Prelude.pure Kernel.Types.APISuccess.Success

postFleetManagementFleetLinkSendOtp :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> API.Types.ProviderPlatform.Operator.FleetManagement.FleetOwnerSendOtpReq -> Environment.Flow API.Types.ProviderPlatform.Operator.FleetManagement.FleetOwnerSendOtpRes)
postFleetManagementFleetLinkSendOtp a4 a3 a2 a1 = do
  mbDashboardPersonId <- Tools.Auth.DashboardRegistration.dashboardPersonIdByMobile a1.mobileNumber a1.mobileCountryCode
  res <- Domain.Action.Dashboard.Operator.FleetManagement.postFleetManagementFleetLinkSendOtp a4 a3 mbDashboardPersonId (Tools.Auth.DashboardUserAuth.dashboardRequestorId a2) a1
  Kernel.Prelude.when (Kernel.Prelude.isNothing mbDashboardPersonId) $
    Tools.Auth.DashboardRegistration.createFleetOwnerDashboardPerson a4.getShortId a3 a1.mobileNumber a1.mobileCountryCode res.fleetOwnerId.getId
  Kernel.Prelude.pure res
