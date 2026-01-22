{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.ProviderPlatform.Operator.Registration
  ( API,
    handler,
  )
where

import qualified API.Types.ProviderPlatform.Operator
import qualified API.Types.ProviderPlatform.Operator.Registration
import qualified Domain.Action.ProviderPlatform.Operator.Registration
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified "lib-dashboard" Environment
import EulerHS.Prelude hiding (sortOn)
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common hiding (INFO)
import Servant
import Storage.Beam.CommonInstances ()
import Tools.Auth.Api

type API = ("operator" :> (PostOperatorRegister :<|> PostRegistrationDashboardRegister))

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = postOperatorRegister merchantId city :<|> postRegistrationDashboardRegister merchantId city

type PostOperatorRegister =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_OPERATOR / 'API.Types.ProviderPlatform.Operator.REGISTRATION / 'API.Types.ProviderPlatform.Operator.Registration.POST_OPERATOR_REGISTER)
      :> API.Types.ProviderPlatform.Operator.Registration.PostOperatorRegister
  )

type PostRegistrationDashboardRegister =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_OPERATOR / 'API.Types.ProviderPlatform.Operator.REGISTRATION / 'API.Types.ProviderPlatform.Operator.Registration.POST_REGISTRATION_DASHBOARD_REGISTER)
      :> API.Types.ProviderPlatform.Operator.Registration.PostRegistrationDashboardRegister
  )

postOperatorRegister :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> API.Types.ProviderPlatform.Operator.Registration.OperatorRegisterReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postOperatorRegister merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Operator.Registration.postOperatorRegister merchantShortId opCity apiTokenInfo req

postRegistrationDashboardRegister :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> API.Types.ProviderPlatform.Operator.Registration.CreateDashboardOperatorReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postRegistrationDashboardRegister merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Operator.Registration.postRegistrationDashboardRegister merchantShortId opCity apiTokenInfo req
