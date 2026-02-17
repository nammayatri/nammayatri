{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.ProviderPlatform.AppManagement where

import qualified API.Action.ProviderPlatform.AppManagement.Driver
import qualified API.Action.ProviderPlatform.AppManagement.DriverSubscription
import qualified API.Action.ProviderPlatform.AppManagement.DriverWallet
import qualified API.Action.ProviderPlatform.AppManagement.Overlay
import qualified API.Action.ProviderPlatform.AppManagement.Penalty
import qualified API.Action.ProviderPlatform.AppManagement.Subscription
import qualified API.Action.ProviderPlatform.AppManagement.SubscriptionTransaction
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified "lib-dashboard" Environment
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Servant

type API = (API.Action.ProviderPlatform.AppManagement.Driver.API :<|> API.Action.ProviderPlatform.AppManagement.DriverSubscription.API :<|> API.Action.ProviderPlatform.AppManagement.DriverWallet.API :<|> API.Action.ProviderPlatform.AppManagement.Overlay.API :<|> API.Action.ProviderPlatform.AppManagement.Penalty.API :<|> API.Action.ProviderPlatform.AppManagement.Subscription.API :<|> API.Action.ProviderPlatform.AppManagement.SubscriptionTransaction.API)

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = API.Action.ProviderPlatform.AppManagement.Driver.handler merchantId city :<|> API.Action.ProviderPlatform.AppManagement.DriverSubscription.handler merchantId city :<|> API.Action.ProviderPlatform.AppManagement.DriverWallet.handler merchantId city :<|> API.Action.ProviderPlatform.AppManagement.Overlay.handler merchantId city :<|> API.Action.ProviderPlatform.AppManagement.Penalty.handler merchantId city :<|> API.Action.ProviderPlatform.AppManagement.Subscription.handler merchantId city :<|> API.Action.ProviderPlatform.AppManagement.SubscriptionTransaction.handler merchantId city
