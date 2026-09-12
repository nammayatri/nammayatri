{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.DashboardAuth.Operator.Registration
  ( API,
    handler,
  )
where

import qualified API.Types.ProviderPlatform.Operator.Registration
import qualified Domain.Action.Dashboard.Operator.Registration
import qualified Domain.Types.Merchant
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Tools.Auth
import qualified Tools.Auth.DashboardRegistration
import Tools.Auth.DashboardUserAuth

type API = ("operator" :> (PostOperatorRegister :<|> PostRegistrationDashboardRegister))

type PostOperatorRegister =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_OPERATOR/REGISTRATION/POST_OPERATOR_REGISTER"
      :> API.Types.ProviderPlatform.Operator.Registration.PostOperatorRegister
  )

type PostRegistrationDashboardRegister =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_OPERATOR/REGISTRATION/POST_REGISTRATION_DASHBOARD_REGISTER"
      :> API.Types.ProviderPlatform.Operator.Registration.PostRegistrationDashboardRegister
  )

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = postOperatorRegister merchantId city :<|> postRegistrationDashboardRegister merchantId city

postOperatorRegister :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> API.Types.ProviderPlatform.Operator.Registration.OperatorRegisterReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postOperatorRegister a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ do
  Tools.Auth.DashboardRegistration.assertOperatorRegistrable a4.getShortId a1.email a1.mobileNumber a1.mobileCountryCode Kernel.Prelude.Nothing
  res <- Domain.Action.Dashboard.Operator.Registration.postOperatorRegister a4 a3 a1
  Tools.Auth.DashboardRegistration.registerOperatorDashboardPerson a4.getShortId a3 a1.email a1.mobileNumber a1.mobileCountryCode a1.firstName a1.lastName Kernel.Prelude.Nothing res.personId.getId Kernel.Prelude.Nothing
  Kernel.Prelude.pure Kernel.Types.APISuccess.Success

postRegistrationDashboardRegister :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> API.Types.ProviderPlatform.Operator.Registration.CreateDashboardOperatorReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postRegistrationDashboardRegister a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ do
  Tools.Auth.DashboardRegistration.assertOperatorRegistrable a4.getShortId (Kernel.Prelude.Just a1.email) a1.mobileNumber a1.mobileCountryCode (Kernel.Prelude.Just a1.roleId)
  res <- Domain.Action.Dashboard.Operator.Registration.postRegistrationDashboardRegister a4 a3 a1
  Tools.Auth.DashboardRegistration.registerOperatorDashboardPerson a4.getShortId a3 (Kernel.Prelude.Just a1.email) a1.mobileNumber a1.mobileCountryCode a1.firstName a1.lastName (Kernel.Prelude.Just a1.password) res.personId.getId (Kernel.Prelude.Just a1.roleId)
  Kernel.Prelude.pure Kernel.Types.APISuccess.Success
