{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.DashboardAuth.Management
  ( API,
    handler,
  )
where

import qualified API.Action.DashboardAuth.Management.AlertIncident
import qualified API.Action.DashboardAuth.Management.Booking
import qualified API.Action.DashboardAuth.Management.Customer
import qualified API.Action.DashboardAuth.Management.FRFSAlerts
import qualified API.Action.DashboardAuth.Management.FRFSTicket
import qualified API.Action.DashboardAuth.Management.Invoice
import qualified API.Action.DashboardAuth.Management.Media
import qualified API.Action.DashboardAuth.Management.Merchant
import qualified API.Action.DashboardAuth.Management.NammaTag
import qualified API.Action.DashboardAuth.Management.Notification
import qualified API.Action.DashboardAuth.Management.Offer
import qualified API.Action.DashboardAuth.Management.Payout
import qualified API.Action.DashboardAuth.Management.Rewards
import qualified API.Action.DashboardAuth.Management.Ride
import qualified API.Action.DashboardAuth.Management.SearchTry
import qualified API.Action.DashboardAuth.Management.Sos
import qualified API.Action.DashboardAuth.Management.SosMedia
import qualified API.Action.DashboardAuth.Management.System
import qualified Domain.Types.Merchant
import qualified Environment
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Servant

type API = (API.Action.DashboardAuth.Management.AlertIncident.API :<|> API.Action.DashboardAuth.Management.Booking.API :<|> API.Action.DashboardAuth.Management.Customer.API :<|> API.Action.DashboardAuth.Management.FRFSAlerts.API :<|> API.Action.DashboardAuth.Management.FRFSTicket.API :<|> API.Action.DashboardAuth.Management.Invoice.API :<|> API.Action.DashboardAuth.Management.Media.API :<|> API.Action.DashboardAuth.Management.Merchant.API :<|> API.Action.DashboardAuth.Management.NammaTag.API :<|> API.Action.DashboardAuth.Management.Notification.API :<|> API.Action.DashboardAuth.Management.Offer.API :<|> API.Action.DashboardAuth.Management.Payout.API :<|> API.Action.DashboardAuth.Management.Rewards.API :<|> API.Action.DashboardAuth.Management.Ride.API :<|> API.Action.DashboardAuth.Management.SearchTry.API :<|> API.Action.DashboardAuth.Management.Sos.API :<|> API.Action.DashboardAuth.Management.SosMedia.API :<|> API.Action.DashboardAuth.Management.System.API)

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = API.Action.DashboardAuth.Management.AlertIncident.handler merchantId city :<|> API.Action.DashboardAuth.Management.Booking.handler merchantId city :<|> API.Action.DashboardAuth.Management.Customer.handler merchantId city :<|> API.Action.DashboardAuth.Management.FRFSAlerts.handler merchantId city :<|> API.Action.DashboardAuth.Management.FRFSTicket.handler merchantId city :<|> API.Action.DashboardAuth.Management.Invoice.handler merchantId city :<|> API.Action.DashboardAuth.Management.Media.handler merchantId city :<|> API.Action.DashboardAuth.Management.Merchant.handler merchantId city :<|> API.Action.DashboardAuth.Management.NammaTag.handler merchantId city :<|> API.Action.DashboardAuth.Management.Notification.handler merchantId city :<|> API.Action.DashboardAuth.Management.Offer.handler merchantId city :<|> API.Action.DashboardAuth.Management.Payout.handler merchantId city :<|> API.Action.DashboardAuth.Management.Rewards.handler merchantId city :<|> API.Action.DashboardAuth.Management.Ride.handler merchantId city :<|> API.Action.DashboardAuth.Management.SearchTry.handler merchantId city :<|> API.Action.DashboardAuth.Management.Sos.handler merchantId city :<|> API.Action.DashboardAuth.Management.SosMedia.handler merchantId city :<|> API.Action.DashboardAuth.Management.System.handler merchantId city
