{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.DashboardAuth.Operator.FleetManagement
  ( API,
    handler,
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

type API = ("operator" :> (GetFleetManagementFleets :<|> PostFleetManagementFleetCreate :<|> PostFleetManagementFleetRegister :<|> PostFleetManagementFleetLinkSendOtp :<|> PostFleetManagementFleetLinkVerifyOtp :<|> PostFleetManagementFleetUnlink :<|> PostFleetManagementFleetMemberAssociationCreateHelper))

type GetFleetManagementFleets =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_OPERATOR/FLEET_MANAGEMENT/GET_FLEET_MANAGEMENT_FLEETS"
      :> API.Types.ProviderPlatform.Operator.FleetManagement.GetFleetManagementFleets
  )

type PostFleetManagementFleetCreate =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_OPERATOR/FLEET_MANAGEMENT/POST_FLEET_MANAGEMENT_FLEET_CREATE"
      :> API.Types.ProviderPlatform.Operator.FleetManagement.PostFleetManagementFleetCreate
  )

type PostFleetManagementFleetRegister =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_OPERATOR/FLEET_MANAGEMENT/POST_FLEET_MANAGEMENT_FLEET_REGISTER"
      :> API.Types.ProviderPlatform.Operator.FleetManagement.PostFleetManagementFleetRegister
  )

type PostFleetManagementFleetLinkSendOtp =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_OPERATOR/FLEET_MANAGEMENT/POST_FLEET_MANAGEMENT_FLEET_LINK_SEND_OTP"
      :> API.Types.ProviderPlatform.Operator.FleetManagement.PostFleetManagementFleetLinkSendOtp
  )

type PostFleetManagementFleetLinkVerifyOtp =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_OPERATOR/FLEET_MANAGEMENT/POST_FLEET_MANAGEMENT_FLEET_LINK_VERIFY_OTP"
      :> API.Types.ProviderPlatform.Operator.FleetManagement.PostFleetManagementFleetLinkVerifyOtp
  )

type PostFleetManagementFleetUnlink =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_OPERATOR/FLEET_MANAGEMENT/POST_FLEET_MANAGEMENT_FLEET_UNLINK"
      :> API.Types.ProviderPlatform.Operator.FleetManagement.PostFleetManagementFleetUnlink
  )

type PostFleetManagementFleetMemberAssociationCreateHelper =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_OPERATOR/FLEET_MANAGEMENT/POST_FLEET_MANAGEMENT_FLEET_MEMBER_ASSOCIATION_CREATE"
      :> API.Types.ProviderPlatform.Operator.FleetManagement.PostFleetManagementFleetMemberAssociationCreateHelper
  )

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = getFleetManagementFleets merchantId city :<|> postFleetManagementFleetCreate merchantId city :<|> postFleetManagementFleetRegister merchantId city :<|> postFleetManagementFleetLinkSendOtp merchantId city :<|> postFleetManagementFleetLinkVerifyOtp merchantId city :<|> postFleetManagementFleetUnlink merchantId city :<|> postFleetManagementFleetMemberAssociationCreate merchantId city

getFleetManagementFleets :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (Kernel.Prelude.Bool) -> Kernel.Prelude.Maybe (Kernel.Prelude.Bool) -> Kernel.Prelude.Maybe (Kernel.Prelude.Bool) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Environment.FlowHandler API.Types.ProviderPlatform.Operator.FleetManagement.FleetInfoRes)
getFleetManagementFleets a9 a8 a7 a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Operator.FleetManagement.getFleetManagementFleets a9 a8 a6 a5 a4 a3 a2 a1 (Tools.Auth.DashboardUserAuth.dashboardRequestorId a7)

postFleetManagementFleetCreate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> API.Types.ProviderPlatform.Fleet.Endpoints.RegistrationV2.FleetOwnerLoginReqV2 -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postFleetManagementFleetCreate a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ do
  mbDashboardPersonId <- Tools.Auth.DashboardRegistration.dashboardPersonIdByMobile a1.mobileNumber a1.mobileCountryCode
  enabled <- Tools.Auth.DashboardRegistration.fleetOnboardingEnabled a4.getShortId a3
  res <- Domain.Action.Dashboard.Operator.FleetManagement.postFleetManagementFleetCreate a4 a3 (Kernel.Prelude.Just enabled) mbDashboardPersonId (Tools.Auth.DashboardUserAuth.dashboardRequestorId a2) a1
  Kernel.Prelude.when (Kernel.Prelude.isNothing mbDashboardPersonId) $
    Tools.Auth.DashboardRegistration.createFleetOwnerDashboardPerson a4.getShortId a3 a1.mobileNumber a1.mobileCountryCode res.personId.getId
  Kernel.Prelude.pure Kernel.Types.APISuccess.Success

postFleetManagementFleetRegister :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> API.Types.ProviderPlatform.Fleet.Endpoints.RegistrationV2.FleetOwnerRegisterReqV2 -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postFleetManagementFleetRegister a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ do
  reg <- Tools.Auth.DashboardUserAuth.beginFleetOwnerRegistration a2 a3 a1.email (Kernel.Types.Id.getId Kernel.Prelude.<$> a1.personId)
  Kernel.Utils.Validation.runRequestValidation (if reg.strongNameCheck then Dashboard.ProviderPlatform.Fleet.RegistrationV2.validateRegisterReqV2 else Dashboard.ProviderPlatform.Fleet.RegistrationV2.validateRegisterReqWithLooseCheck) a1
  let req = a1 {API.Types.ProviderPlatform.Fleet.Endpoints.RegistrationV2.adminApprovalRequired = reg.adminApprovalRequired, API.Types.ProviderPlatform.Fleet.Endpoints.RegistrationV2.email = reg.normalizedEmail} :: API.Types.ProviderPlatform.Fleet.Endpoints.RegistrationV2.FleetOwnerRegisterReqV2
  res <- Domain.Action.Dashboard.Operator.FleetManagement.postFleetManagementFleetRegister a4 a3 reg.requestorId req
  Tools.Auth.DashboardUserAuth.completeFleetOwnerRegistration reg.fleetOwnerId reg.normalizedEmail a1.firstName a1.lastName (a1.fleetType Kernel.Prelude.== Kernel.Prelude.Just API.Types.ProviderPlatform.Fleet.Endpoints.RegistrationV2.RENTAL_FLEET) res.enabled
  Kernel.Prelude.pure Kernel.Types.APISuccess.Success

postFleetManagementFleetLinkSendOtp :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> API.Types.ProviderPlatform.Operator.FleetManagement.FleetOwnerSendOtpReq -> Environment.FlowHandler API.Types.ProviderPlatform.Operator.FleetManagement.FleetOwnerSendOtpRes)
postFleetManagementFleetLinkSendOtp a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ do
  mbDashboardPersonId <- Tools.Auth.DashboardRegistration.dashboardPersonIdByMobile a1.mobileNumber a1.mobileCountryCode
  res <- Domain.Action.Dashboard.Operator.FleetManagement.postFleetManagementFleetLinkSendOtp a4 a3 mbDashboardPersonId (Tools.Auth.DashboardUserAuth.dashboardRequestorId a2) a1
  Kernel.Prelude.when (Kernel.Prelude.isNothing mbDashboardPersonId) $
    Tools.Auth.DashboardRegistration.createFleetOwnerDashboardPerson a4.getShortId a3 a1.mobileNumber a1.mobileCountryCode res.fleetOwnerId.getId
  Kernel.Prelude.pure res

postFleetManagementFleetLinkVerifyOtp :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> API.Types.ProviderPlatform.Operator.FleetManagement.FleetOwnerVerifyOtpReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postFleetManagementFleetLinkVerifyOtp a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Operator.FleetManagement.postFleetManagementFleetLinkVerifyOtp a4 a3 (Tools.Auth.DashboardUserAuth.dashboardRequestorId a2) a1

postFleetManagementFleetUnlink :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Text -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postFleetManagementFleetUnlink a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Operator.FleetManagement.postFleetManagementFleetUnlink a4 a3 a1 (Tools.Auth.DashboardUserAuth.dashboardRequestorId a2)

postFleetManagementFleetMemberAssociationCreate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> API.Types.ProviderPlatform.Operator.FleetManagement.FleetMemberAssociationCreateReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postFleetManagementFleetMemberAssociationCreate a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Operator.FleetManagement.postFleetManagementFleetMemberAssociationCreate a4 a3 a1
