{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.DashboardAuth.Fleet.RegistrationV2
  ( API,
    handler,
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

type API = ("fleet" :> (PostRegistrationV2LoginOtp :<|> PostRegistrationV2VerifyOtp :<|> PostRegistrationV2Register :<|> PostRegistrationV2RegisterBankAccountLink :<|> GetRegistrationV2RegisterBankAccountStatus :<|> PutRegistrationV2ProfileLanguage :<|> GetRegistrationV2ProfileLanguage))

type PostRegistrationV2LoginOtp = API.Types.ProviderPlatform.Fleet.RegistrationV2.PostRegistrationV2LoginOtp

type PostRegistrationV2VerifyOtp = API.Types.ProviderPlatform.Fleet.RegistrationV2.PostRegistrationV2VerifyOtp

type PostRegistrationV2Register =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_FLEET/REGISTRATION_V2/POST_REGISTRATION_V2_REGISTER"
      :> API.Types.ProviderPlatform.Fleet.RegistrationV2.PostRegistrationV2Register
  )

type PostRegistrationV2RegisterBankAccountLink =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_FLEET/REGISTRATION_V2/POST_REGISTRATION_V2_REGISTER_BANK_ACCOUNT_LINK"
      :> API.Types.ProviderPlatform.Fleet.RegistrationV2.PostRegistrationV2RegisterBankAccountLink
  )

type GetRegistrationV2RegisterBankAccountStatus =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_FLEET/REGISTRATION_V2/GET_REGISTRATION_V2_REGISTER_BANK_ACCOUNT_STATUS"
      :> API.Types.ProviderPlatform.Fleet.RegistrationV2.GetRegistrationV2RegisterBankAccountStatus
  )

type PutRegistrationV2ProfileLanguage =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_FLEET/REGISTRATION_V2/PUT_REGISTRATION_V2_PROFILE_LANGUAGE"
      :> API.Types.ProviderPlatform.Fleet.RegistrationV2.PutRegistrationV2ProfileLanguage
  )

type GetRegistrationV2ProfileLanguage =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_FLEET/REGISTRATION_V2/GET_REGISTRATION_V2_PROFILE_LANGUAGE"
      :> API.Types.ProviderPlatform.Fleet.RegistrationV2.GetRegistrationV2ProfileLanguage
  )

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = postRegistrationV2LoginOtp merchantId city :<|> postRegistrationV2VerifyOtp merchantId city :<|> postRegistrationV2Register merchantId city :<|> postRegistrationV2RegisterBankAccountLink merchantId city :<|> getRegistrationV2RegisterBankAccountStatus merchantId city :<|> putRegistrationV2ProfileLanguage merchantId city :<|> getRegistrationV2ProfileLanguage merchantId city

postRegistrationV2LoginOtp :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> API.Types.ProviderPlatform.Fleet.RegistrationV2.FleetOwnerLoginReqV2 -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postRegistrationV2LoginOtp a3 a2 a1 = withDashboardFlowHandlerAPI $ do
  mbDashboardPersonId <- Tools.Auth.DashboardRegistration.dashboardPersonIdByMobile a1.mobileNumber a1.mobileCountryCode
  enabled <- Tools.Auth.DashboardRegistration.verifyFleetWhileLogin a3.getShortId a2
  res <- Domain.Action.Dashboard.Fleet.RegistrationV2.postRegistrationV2LoginOtp a3 a2 mbDashboardPersonId enabled a1
  Kernel.Prelude.when (Kernel.Prelude.isNothing mbDashboardPersonId) $
    Tools.Auth.DashboardRegistration.createFleetOwnerDashboardPerson a3.getShortId a2 a1.mobileNumber a1.mobileCountryCode res.personId.getId
  Kernel.Prelude.pure Kernel.Types.APISuccess.Success

postRegistrationV2VerifyOtp :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> API.Types.ProviderPlatform.Fleet.RegistrationV2.FleetOwnerVerifyReqV2 -> Environment.FlowHandler API.Types.ProviderPlatform.Fleet.RegistrationV2.FleetOwnerVerifyResV2)
postRegistrationV2VerifyOtp a3 a2 a1 = withDashboardFlowHandlerAPI $ do
  _ <- Domain.Action.Dashboard.Fleet.RegistrationV2.postRegistrationV2VerifyOtp a3 a2 a1
  authToken <- Tools.Auth.DashboardRegistration.issueFleetOwnerAuthToken a3.getShortId a2 a1.mobileNumber a1.mobileCountryCode
  Kernel.Prelude.pure API.Types.ProviderPlatform.Fleet.RegistrationV2.FleetOwnerVerifyResV2 {API.Types.ProviderPlatform.Fleet.RegistrationV2.authToken = authToken}

postRegistrationV2Register :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> API.Types.ProviderPlatform.Fleet.RegistrationV2.FleetOwnerRegisterReqV2 -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postRegistrationV2Register a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ do
  reg <- Tools.Auth.DashboardUserAuth.beginFleetOwnerRegistration a2 a3 a1.email (Kernel.Types.Id.getId Kernel.Prelude.<$> a1.personId)
  Kernel.Utils.Validation.runRequestValidation (if reg.strongNameCheck then Dashboard.ProviderPlatform.Fleet.RegistrationV2.validateRegisterReqV2 else Dashboard.ProviderPlatform.Fleet.RegistrationV2.validateRegisterReqWithLooseCheck) a1
  let req = a1 {API.Types.ProviderPlatform.Fleet.RegistrationV2.adminApprovalRequired = reg.adminApprovalRequired, API.Types.ProviderPlatform.Fleet.RegistrationV2.email = reg.normalizedEmail} :: API.Types.ProviderPlatform.Fleet.RegistrationV2.FleetOwnerRegisterReqV2
  res <- Domain.Action.Dashboard.Fleet.RegistrationV2.postRegistrationV2Register a4 a3 reg.requestorId req
  Tools.Auth.DashboardUserAuth.completeFleetOwnerRegistration reg.fleetOwnerId reg.normalizedEmail a1.firstName a1.lastName (a1.fleetType Kernel.Prelude.== Kernel.Prelude.Just API.Types.ProviderPlatform.Fleet.RegistrationV2.RENTAL_FLEET) res.enabled
  Kernel.Prelude.pure Kernel.Types.APISuccess.Success

postRegistrationV2RegisterBankAccountLink :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Domain.Types.PaymentMode.PaymentMode) -> Kernel.Prelude.Maybe (Domain.Types.InitiatedBy.InitiatedBy) -> Environment.FlowHandler API.Types.ProviderPlatform.Fleet.RegistrationV2.FleetBankAccountLinkResp)
postRegistrationV2RegisterBankAccountLink a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Fleet.RegistrationV2.postRegistrationV2RegisterBankAccountLink a6 a5 a3 a2 a1 (Tools.Auth.DashboardUserAuth.dashboardRequestorId a4)

getRegistrationV2RegisterBankAccountStatus :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Bool) -> Environment.FlowHandler API.Types.ProviderPlatform.Fleet.RegistrationV2.FleetBankAccountResp)
getRegistrationV2RegisterBankAccountStatus a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Fleet.RegistrationV2.getRegistrationV2RegisterBankAccountStatus a5 a4 a2 a1 (Tools.Auth.DashboardUserAuth.dashboardRequestorId a3)

putRegistrationV2ProfileLanguage :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> API.Types.ProviderPlatform.Fleet.RegistrationV2.FleetOwnerUpdateLanguageReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
putRegistrationV2ProfileLanguage a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ do
  _ <- Domain.Action.Dashboard.Fleet.RegistrationV2.putRegistrationV2ProfileLanguage a4 a3 (Tools.Auth.DashboardUserAuth.dashboardRequestorId a2) a1
  Tools.Auth.DashboardRegistration.updateDashboardPersonLanguage (Tools.Auth.DashboardUserAuth.dashboardRequestorId a2) a1.language
  Kernel.Prelude.pure Kernel.Types.APISuccess.Success

getRegistrationV2ProfileLanguage :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Environment.FlowHandler API.Types.ProviderPlatform.Fleet.RegistrationV2.FleetOwnerLanguageRes)
getRegistrationV2ProfileLanguage a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Fleet.RegistrationV2.getRegistrationV2ProfileLanguage a3 a2 (Tools.Auth.DashboardUserAuth.dashboardRequestorId a1)
