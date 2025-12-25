{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.ProviderPlatform.AppManagement.DriverSubscription
  ( API,
    handler,
  )
where

import qualified "dynamic-offer-driver-app" API.Types.Dashboard.AppManagement
import qualified "dynamic-offer-driver-app" API.Types.Dashboard.AppManagement.DriverSubscription
import qualified Dashboard.Common
import qualified Domain.Action.ProviderPlatform.AppManagement.DriverSubscription
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

type API = ("driver" :> (PostDriverSubscriptionSendSms :<|> PostDriverSubscriptionUpdateDriverFeeAndInvoiceInfo))

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = postDriverSubscriptionSendSms merchantId city :<|> postDriverSubscriptionUpdateDriverFeeAndInvoiceInfo merchantId city

type PostDriverSubscriptionSendSms =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_APP_MANAGEMENT / 'API.Types.Dashboard.AppManagement.DRIVER_SUBSCRIPTION / 'API.Types.Dashboard.AppManagement.DriverSubscription.POST_DRIVER_SUBSCRIPTION_SEND_SMS)
      :> API.Types.Dashboard.AppManagement.DriverSubscription.PostDriverSubscriptionSendSms
  )

type PostDriverSubscriptionUpdateDriverFeeAndInvoiceInfo =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_APP_MANAGEMENT / 'API.Types.Dashboard.AppManagement.DRIVER_SUBSCRIPTION / 'API.Types.Dashboard.AppManagement.DriverSubscription.POST_DRIVER_SUBSCRIPTION_UPDATE_DRIVER_FEE_AND_INVOICE_INFO)
      :> API.Types.Dashboard.AppManagement.DriverSubscription.PostDriverSubscriptionUpdateDriverFeeAndInvoiceInfo
  )

postDriverSubscriptionSendSms :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Dashboard.Common.Driver -> API.Types.Dashboard.AppManagement.DriverSubscription.SendSmsReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postDriverSubscriptionSendSms merchantShortId opCity apiTokenInfo driverId req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.AppManagement.DriverSubscription.postDriverSubscriptionSendSms merchantShortId opCity apiTokenInfo driverId req

postDriverSubscriptionUpdateDriverFeeAndInvoiceInfo :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Types.Id.Id Dashboard.Common.Driver -> Dashboard.Common.ServiceNames -> API.Types.Dashboard.AppManagement.DriverSubscription.SubscriptionDriverFeesAndInvoicesToUpdate -> Environment.FlowHandler API.Types.Dashboard.AppManagement.DriverSubscription.SubscriptionDriverFeesAndInvoicesToUpdate)
postDriverSubscriptionUpdateDriverFeeAndInvoiceInfo merchantShortId opCity apiTokenInfo driverId serviceName req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.AppManagement.DriverSubscription.postDriverSubscriptionUpdateDriverFeeAndInvoiceInfo merchantShortId opCity apiTokenInfo driverId serviceName req
