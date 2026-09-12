{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.DashboardAuth.AppManagement
  ( API,
    handler,
  )
where

import qualified API.Action.DashboardAuth.AppManagement.Customer
import qualified API.Action.DashboardAuth.AppManagement.EDCMachine
import qualified API.Action.DashboardAuth.AppManagement.EventManagement
import qualified API.Action.DashboardAuth.AppManagement.FRFSTicketService
import qualified API.Action.DashboardAuth.AppManagement.MerchantOnboarding
import qualified API.Action.DashboardAuth.AppManagement.Pass
import qualified API.Action.DashboardAuth.AppManagement.PassOrganization
import qualified API.Action.DashboardAuth.AppManagement.Passetto
import qualified API.Action.DashboardAuth.AppManagement.Payment
import qualified API.Action.DashboardAuth.AppManagement.SeatLayout
import qualified API.Action.DashboardAuth.AppManagement.StopRouteDetails
import qualified API.Action.DashboardAuth.AppManagement.TicketDashboard
import qualified API.Action.DashboardAuth.AppManagement.Tickets
import qualified API.Action.DashboardAuth.AppManagement.TransitOperator
import qualified API.Action.DashboardAuth.AppManagement.VehicleSeatLayoutMapping
import qualified Domain.Types.Merchant
import qualified Environment
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Servant

type API = (API.Action.DashboardAuth.AppManagement.Customer.API :<|> API.Action.DashboardAuth.AppManagement.EDCMachine.API :<|> API.Action.DashboardAuth.AppManagement.EventManagement.API :<|> API.Action.DashboardAuth.AppManagement.FRFSTicketService.API :<|> API.Action.DashboardAuth.AppManagement.MerchantOnboarding.API :<|> API.Action.DashboardAuth.AppManagement.Pass.API :<|> API.Action.DashboardAuth.AppManagement.PassOrganization.API :<|> API.Action.DashboardAuth.AppManagement.Passetto.API :<|> API.Action.DashboardAuth.AppManagement.Payment.API :<|> API.Action.DashboardAuth.AppManagement.SeatLayout.API :<|> API.Action.DashboardAuth.AppManagement.StopRouteDetails.API :<|> API.Action.DashboardAuth.AppManagement.TicketDashboard.API :<|> API.Action.DashboardAuth.AppManagement.Tickets.API :<|> API.Action.DashboardAuth.AppManagement.TransitOperator.API :<|> API.Action.DashboardAuth.AppManagement.VehicleSeatLayoutMapping.API)

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = API.Action.DashboardAuth.AppManagement.Customer.handler merchantId city :<|> API.Action.DashboardAuth.AppManagement.EDCMachine.handler merchantId city :<|> API.Action.DashboardAuth.AppManagement.EventManagement.handler merchantId city :<|> API.Action.DashboardAuth.AppManagement.FRFSTicketService.handler merchantId city :<|> API.Action.DashboardAuth.AppManagement.MerchantOnboarding.handler merchantId city :<|> API.Action.DashboardAuth.AppManagement.Pass.handler merchantId city :<|> API.Action.DashboardAuth.AppManagement.PassOrganization.handler merchantId city :<|> API.Action.DashboardAuth.AppManagement.Passetto.handler merchantId city :<|> API.Action.DashboardAuth.AppManagement.Payment.handler merchantId city :<|> API.Action.DashboardAuth.AppManagement.SeatLayout.handler merchantId city :<|> API.Action.DashboardAuth.AppManagement.StopRouteDetails.handler merchantId city :<|> API.Action.DashboardAuth.AppManagement.TicketDashboard.handler merchantId city :<|> API.Action.DashboardAuth.AppManagement.Tickets.handler merchantId city :<|> API.Action.DashboardAuth.AppManagement.TransitOperator.handler merchantId city :<|> API.Action.DashboardAuth.AppManagement.VehicleSeatLayoutMapping.handler merchantId city
