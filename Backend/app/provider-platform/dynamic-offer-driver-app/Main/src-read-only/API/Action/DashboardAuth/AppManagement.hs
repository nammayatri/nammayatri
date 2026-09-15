{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.DashboardAuth.AppManagement
  ( API,
    handler,
  )
where

import qualified API.Action.DashboardAuth.AppManagement.Driver
import qualified API.Action.DashboardAuth.AppManagement.DriverSubscription
import qualified API.Action.DashboardAuth.AppManagement.DriverWallet
import qualified API.Action.DashboardAuth.AppManagement.FrfsFleetOperator
import qualified API.Action.DashboardAuth.AppManagement.Overlay
import qualified API.Action.DashboardAuth.AppManagement.Penalty
import qualified API.Action.DashboardAuth.AppManagement.Subscription
import qualified API.Action.DashboardAuth.AppManagement.SubscriptionTransaction
import qualified Domain.Types.Merchant
import qualified Environment
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Servant

type API = (API.Action.DashboardAuth.AppManagement.Driver.API :<|> API.Action.DashboardAuth.AppManagement.DriverSubscription.API :<|> API.Action.DashboardAuth.AppManagement.DriverWallet.API :<|> API.Action.DashboardAuth.AppManagement.FrfsFleetOperator.API :<|> API.Action.DashboardAuth.AppManagement.Overlay.API :<|> API.Action.DashboardAuth.AppManagement.Penalty.API :<|> API.Action.DashboardAuth.AppManagement.Subscription.API :<|> API.Action.DashboardAuth.AppManagement.SubscriptionTransaction.API)

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = API.Action.DashboardAuth.AppManagement.Driver.handler merchantId city :<|> API.Action.DashboardAuth.AppManagement.DriverSubscription.handler merchantId city :<|> API.Action.DashboardAuth.AppManagement.DriverWallet.handler merchantId city :<|> API.Action.DashboardAuth.AppManagement.FrfsFleetOperator.handler merchantId city :<|> API.Action.DashboardAuth.AppManagement.Overlay.handler merchantId city :<|> API.Action.DashboardAuth.AppManagement.Penalty.handler merchantId city :<|> API.Action.DashboardAuth.AppManagement.Subscription.handler merchantId city :<|> API.Action.DashboardAuth.AppManagement.SubscriptionTransaction.handler merchantId city
