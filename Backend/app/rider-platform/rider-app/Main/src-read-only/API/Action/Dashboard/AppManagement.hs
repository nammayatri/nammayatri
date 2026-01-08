{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.Dashboard.AppManagement
  ( API,
    handler,
  )
where

import qualified API.Action.Dashboard.AppManagement.Customer
import qualified API.Action.Dashboard.AppManagement.EventManagement
import qualified API.Action.Dashboard.AppManagement.MerchantOnboarding
import qualified API.Action.Dashboard.AppManagement.Pass
import qualified API.Action.Dashboard.AppManagement.Payment
import qualified API.Action.Dashboard.AppManagement.TicketDashboard
import qualified API.Action.Dashboard.AppManagement.Tickets
import qualified Domain.Types.Merchant
import qualified Environment
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Servant

type API = (API.Action.Dashboard.AppManagement.Customer.API :<|> API.Action.Dashboard.AppManagement.EventManagement.API :<|> API.Action.Dashboard.AppManagement.MerchantOnboarding.API :<|> API.Action.Dashboard.AppManagement.Pass.API :<|> API.Action.Dashboard.AppManagement.Payment.API :<|> API.Action.Dashboard.AppManagement.TicketDashboard.API :<|> API.Action.Dashboard.AppManagement.Tickets.API)

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = API.Action.Dashboard.AppManagement.Customer.handler merchantId city :<|> API.Action.Dashboard.AppManagement.EventManagement.handler merchantId city :<|> API.Action.Dashboard.AppManagement.MerchantOnboarding.handler merchantId city :<|> API.Action.Dashboard.AppManagement.Pass.handler merchantId city :<|> API.Action.Dashboard.AppManagement.Payment.handler merchantId city :<|> API.Action.Dashboard.AppManagement.TicketDashboard.handler merchantId city :<|> API.Action.Dashboard.AppManagement.Tickets.handler merchantId city
