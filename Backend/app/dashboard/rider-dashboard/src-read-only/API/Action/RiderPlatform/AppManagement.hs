{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.RiderPlatform.AppManagement where

import qualified API.Action.RiderPlatform.AppManagement.Customer
import qualified API.Action.RiderPlatform.AppManagement.EventManagement
import qualified API.Action.RiderPlatform.AppManagement.MerchantOnboarding
import qualified API.Action.RiderPlatform.AppManagement.Pass
import qualified API.Action.RiderPlatform.AppManagement.Payment
import qualified API.Action.RiderPlatform.AppManagement.TicketDashboard
import qualified API.Action.RiderPlatform.AppManagement.Tickets
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified "lib-dashboard" Environment
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Servant

type API = (API.Action.RiderPlatform.AppManagement.Customer.API :<|> API.Action.RiderPlatform.AppManagement.EventManagement.API :<|> API.Action.RiderPlatform.AppManagement.MerchantOnboarding.API :<|> API.Action.RiderPlatform.AppManagement.Pass.API :<|> API.Action.RiderPlatform.AppManagement.Payment.API :<|> API.Action.RiderPlatform.AppManagement.TicketDashboard.API :<|> API.Action.RiderPlatform.AppManagement.Tickets.API)

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = API.Action.RiderPlatform.AppManagement.Customer.handler merchantId city :<|> API.Action.RiderPlatform.AppManagement.EventManagement.handler merchantId city :<|> API.Action.RiderPlatform.AppManagement.MerchantOnboarding.handler merchantId city :<|> API.Action.RiderPlatform.AppManagement.Pass.handler merchantId city :<|> API.Action.RiderPlatform.AppManagement.Payment.handler merchantId city :<|> API.Action.RiderPlatform.AppManagement.TicketDashboard.handler merchantId city :<|> API.Action.RiderPlatform.AppManagement.Tickets.handler merchantId city
