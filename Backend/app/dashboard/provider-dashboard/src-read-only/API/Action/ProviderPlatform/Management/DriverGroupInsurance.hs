{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.ProviderPlatform.Management.DriverGroupInsurance
  ( API,
    handler,
  )
where

import qualified API.Types.ProviderPlatform.Management
import qualified API.Types.ProviderPlatform.Management.DriverGroupInsurance
import qualified Dashboard.Common
import qualified Domain.Action.ProviderPlatform.Management.DriverGroupInsurance
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

type API = ("driverGroupInsurance" :> (PostDriverGroupInsuranceSecondBotCheck :<|> PostDriverGroupInsuranceEnable))

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = postDriverGroupInsuranceSecondBotCheck merchantId city :<|> postDriverGroupInsuranceEnable merchantId city

type PostDriverGroupInsuranceSecondBotCheck =
  ( ApiAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      ('DSL)
      (('PROVIDER_MANAGEMENT) / ('API.Types.ProviderPlatform.Management.DRIVER_GROUP_INSURANCE) / ('API.Types.ProviderPlatform.Management.DriverGroupInsurance.POST_DRIVER_GROUP_INSURANCE_SECOND_BOT_CHECK))
      :> API.Types.ProviderPlatform.Management.DriverGroupInsurance.PostDriverGroupInsuranceSecondBotCheck
  )

type PostDriverGroupInsuranceEnable =
  ( ApiAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      ('DSL)
      (('PROVIDER_MANAGEMENT) / ('API.Types.ProviderPlatform.Management.DRIVER_GROUP_INSURANCE) / ('API.Types.ProviderPlatform.Management.DriverGroupInsurance.POST_DRIVER_GROUP_INSURANCE_ENABLE))
      :> API.Types.ProviderPlatform.Management.DriverGroupInsurance.PostDriverGroupInsuranceEnable
  )

postDriverGroupInsuranceSecondBotCheck :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Dashboard.Common.DriverGroupInsurance -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postDriverGroupInsuranceSecondBotCheck merchantShortId opCity apiTokenInfo insuranceId = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.DriverGroupInsurance.postDriverGroupInsuranceSecondBotCheck merchantShortId opCity apiTokenInfo insuranceId

postDriverGroupInsuranceEnable :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Dashboard.Common.DriverGroupInsurance -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postDriverGroupInsuranceEnable merchantShortId opCity apiTokenInfo insuranceId = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Management.DriverGroupInsurance.postDriverGroupInsuranceEnable merchantShortId opCity apiTokenInfo insuranceId
