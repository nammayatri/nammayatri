{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.Dashboard.AppManagement
  ( API,
    handler,
  )
where

import qualified API.Action.Dashboard.AppManagement.Driver
import qualified API.Action.Dashboard.AppManagement.DriverSubscription
import qualified API.Action.Dashboard.AppManagement.Overlay
import qualified API.Action.Dashboard.AppManagement.Penalty
import qualified API.Action.Dashboard.AppManagement.Subscription
import qualified API.Action.Dashboard.AppManagement.SubscriptionTransaction
import qualified Domain.Types.Merchant
import qualified Environment
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Servant

type API = (API.Action.Dashboard.AppManagement.Driver.API :<|> API.Action.Dashboard.AppManagement.DriverSubscription.API :<|> API.Action.Dashboard.AppManagement.Overlay.API :<|> API.Action.Dashboard.AppManagement.Penalty.API :<|> API.Action.Dashboard.AppManagement.Subscription.API :<|> API.Action.Dashboard.AppManagement.SubscriptionTransaction.API)

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = API.Action.Dashboard.AppManagement.Driver.handler merchantId city :<|> API.Action.Dashboard.AppManagement.DriverSubscription.handler merchantId city :<|> API.Action.Dashboard.AppManagement.Overlay.handler merchantId city :<|> API.Action.Dashboard.AppManagement.Penalty.handler merchantId city :<|> API.Action.Dashboard.AppManagement.Subscription.handler merchantId city :<|> API.Action.Dashboard.AppManagement.SubscriptionTransaction.handler merchantId city
