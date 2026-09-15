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
module Domain.Action.DashboardAuth.Fleet.RegistrationV2
  ( postRegistrationV2LoginOtp,
    postRegistrationV2VerifyOtp,
    postRegistrationV2Register,
    putRegistrationV2ProfileLanguage,
  )
where

import qualified API.Types.ProviderPlatform.Fleet.RegistrationV2
import qualified Dashboard.ProviderPlatform.Fleet.RegistrationV2
import qualified Domain.Action.Dashboard.Fleet.RegistrationV2
import qualified Domain.Types.InitiatedBy
import qualified Domain.Types.Merchant
import qualified Domain.Types.PaymentMode
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

postRegistrationV2LoginOtp :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> API.Types.ProviderPlatform.Fleet.RegistrationV2.FleetOwnerLoginReqV2 -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postRegistrationV2LoginOtp a3 a2 a1 = do
  mbDashboardPersonId <- Tools.Auth.DashboardRegistration.dashboardPersonIdByMobile a1.mobileNumber a1.mobileCountryCode
  enabled <- Tools.Auth.DashboardRegistration.verifyFleetWhileLogin a3.getShortId a2
  res <- Domain.Action.Dashboard.Fleet.RegistrationV2.postRegistrationV2LoginOtp a3 a2 mbDashboardPersonId enabled a1
  Kernel.Prelude.when (Kernel.Prelude.isNothing mbDashboardPersonId) $
    Tools.Auth.DashboardRegistration.createFleetOwnerDashboardPerson a3.getShortId a2 a1.mobileNumber a1.mobileCountryCode res.personId.getId
  Kernel.Prelude.pure Kernel.Types.APISuccess.Success

postRegistrationV2VerifyOtp :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> API.Types.ProviderPlatform.Fleet.RegistrationV2.FleetOwnerVerifyReqV2 -> Environment.Flow API.Types.ProviderPlatform.Fleet.RegistrationV2.FleetOwnerVerifyResV2)
postRegistrationV2VerifyOtp a3 a2 a1 = do
  _ <- Domain.Action.Dashboard.Fleet.RegistrationV2.postRegistrationV2VerifyOtp a3 a2 a1
  authToken <- Tools.Auth.DashboardRegistration.issueFleetOwnerAuthToken a3.getShortId a2 a1.mobileNumber a1.mobileCountryCode
  Kernel.Prelude.pure API.Types.ProviderPlatform.Fleet.RegistrationV2.FleetOwnerVerifyResV2 {API.Types.ProviderPlatform.Fleet.RegistrationV2.authToken = authToken}

postRegistrationV2Register :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> API.Types.ProviderPlatform.Fleet.RegistrationV2.FleetOwnerRegisterReqV2 -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postRegistrationV2Register a4 a3 a2 a1 = do
  reg <- Tools.Auth.DashboardUserAuth.beginFleetOwnerRegistration a2 a3 a1.email (Kernel.Types.Id.getId Kernel.Prelude.<$> a1.personId)
  Kernel.Utils.Validation.runRequestValidation (if reg.strongNameCheck then Dashboard.ProviderPlatform.Fleet.RegistrationV2.validateRegisterReqV2 else Dashboard.ProviderPlatform.Fleet.RegistrationV2.validateRegisterReqWithLooseCheck) a1
  let req = a1 {API.Types.ProviderPlatform.Fleet.RegistrationV2.adminApprovalRequired = reg.adminApprovalRequired, API.Types.ProviderPlatform.Fleet.RegistrationV2.email = reg.normalizedEmail} :: API.Types.ProviderPlatform.Fleet.RegistrationV2.FleetOwnerRegisterReqV2
  res <- Domain.Action.Dashboard.Fleet.RegistrationV2.postRegistrationV2Register a4 a3 reg.requestorId req
  Tools.Auth.DashboardUserAuth.completeFleetOwnerRegistration reg.fleetOwnerId reg.normalizedEmail a1.firstName a1.lastName (a1.fleetType Kernel.Prelude.== Kernel.Prelude.Just API.Types.ProviderPlatform.Fleet.RegistrationV2.RENTAL_FLEET) res.enabled
  Kernel.Prelude.pure Kernel.Types.APISuccess.Success

putRegistrationV2ProfileLanguage :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> API.Types.ProviderPlatform.Fleet.RegistrationV2.FleetOwnerUpdateLanguageReq -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
putRegistrationV2ProfileLanguage a4 a3 a2 a1 = do
  _ <- Domain.Action.Dashboard.Fleet.RegistrationV2.putRegistrationV2ProfileLanguage a4 a3 (Tools.Auth.DashboardUserAuth.dashboardRequestorId a2) a1
  Tools.Auth.DashboardRegistration.updateDashboardPersonLanguage (Tools.Auth.DashboardUserAuth.dashboardRequestorId a2) a1.language
  Kernel.Prelude.pure Kernel.Types.APISuccess.Success
