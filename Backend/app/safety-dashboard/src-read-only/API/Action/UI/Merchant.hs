{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.UI.Merchant
  ( API,
    handler,
  )
where

import qualified API.Types.UI.Merchant
import qualified Domain.Action.UI.Merchant
import qualified Domain.Types.MerchantConfigs
import qualified "lib-dashboard" Environment
import EulerHS.Prelude
import qualified Kernel.Types.APISuccess
import Kernel.Utils.Common
import Servant
import "lib-dashboard" Tools.Auth

type API =
  ( DashboardAuth 'MERCHANT_ADMIN :> "set" :> "merchantConfig" :> ReqBody '[JSON] API.Types.UI.Merchant.SetMerchantConfigReq
      :> Post
           '[JSON]
           Kernel.Types.APISuccess.APISuccess
      :<|> DashboardAuth 'MERCHANT_ADMIN
      :> "merchant"
      :> "user"
      :> "list"
      :> Get
           '[JSON]
           API.Types.UI.Merchant.MerchantUserList
      :<|> DashboardAuth 'MERCHANT_ADMIN
      :> "merchant"
      :> "user"
      :> "resetPassword"
      :> ReqBody
           '[JSON]
           API.Types.UI.Merchant.ResetPasswordReq
      :> Post
           '[JSON]
           Kernel.Types.APISuccess.APISuccess
      :<|> DashboardAuth 'MERCHANT_ADMIN
      :> "merchant"
      :> "get"
      :> "webhookConfig"
      :> Get
           '[JSON]
           Domain.Types.MerchantConfigs.MerchantConfigs
      :<|> DashboardAuth 'MERCHANT_ADMIN
      :> "merchant"
      :> "webhookConfig"
      :> "preference"
      :> ReqBody
           '[JSON]
           API.Types.UI.Merchant.WebHookConfigPreferenceReq
      :> Post
           '[JSON]
           Kernel.Types.APISuccess.APISuccess
  )

handler :: Environment.FlowServer API
handler = postSetMerchantConfig :<|> getMerchantUserList :<|> postMerchantUserResetPassword :<|> getMerchantGetWebhookConfig :<|> postMerchantWebhookConfigPreference

postSetMerchantConfig :: (TokenInfo -> API.Types.UI.Merchant.SetMerchantConfigReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postSetMerchantConfig a2 a1 = withFlowHandlerAPI' $ Domain.Action.UI.Merchant.postSetMerchantConfig a2 a1

getMerchantUserList :: (TokenInfo -> Environment.FlowHandler API.Types.UI.Merchant.MerchantUserList)
getMerchantUserList a1 = withFlowHandlerAPI' $ Domain.Action.UI.Merchant.getMerchantUserList a1

postMerchantUserResetPassword :: (TokenInfo -> API.Types.UI.Merchant.ResetPasswordReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postMerchantUserResetPassword a2 a1 = withFlowHandlerAPI' $ Domain.Action.UI.Merchant.postMerchantUserResetPassword a2 a1

getMerchantGetWebhookConfig :: (TokenInfo -> Environment.FlowHandler Domain.Types.MerchantConfigs.MerchantConfigs)
getMerchantGetWebhookConfig a1 = withFlowHandlerAPI' $ Domain.Action.UI.Merchant.getMerchantGetWebhookConfig a1

postMerchantWebhookConfigPreference :: (TokenInfo -> API.Types.UI.Merchant.WebHookConfigPreferenceReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postMerchantWebhookConfigPreference a2 a1 = withFlowHandlerAPI' $ Domain.Action.UI.Merchant.postMerchantWebhookConfigPreference a2 a1
