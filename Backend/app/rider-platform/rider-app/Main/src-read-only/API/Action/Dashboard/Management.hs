{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.Dashboard.Management
  ( API,
    handler,
  )
where

import qualified API.Action.Dashboard.Management.Account
import qualified API.Action.Dashboard.Management.AlertIncident
import qualified API.Action.Dashboard.Management.Booking
import qualified API.Action.Dashboard.Management.Customer
import qualified API.Action.Dashboard.Management.FRFSAlerts
import qualified API.Action.Dashboard.Management.FRFSTicket
import qualified API.Action.Dashboard.Management.Invoice
import qualified API.Action.Dashboard.Management.Media
import qualified API.Action.Dashboard.Management.Merchant
import qualified API.Action.Dashboard.Management.NammaTag
import qualified API.Action.Dashboard.Management.Ride
import qualified API.Action.Dashboard.Management.Sos
import qualified API.Action.Dashboard.Management.SosMedia
import qualified API.Action.Dashboard.Management.System
import qualified Domain.Types.Merchant
import qualified Environment
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Servant

type API = (API.Action.Dashboard.Management.Account.API :<|> API.Action.Dashboard.Management.AlertIncident.API :<|> API.Action.Dashboard.Management.Booking.API :<|> API.Action.Dashboard.Management.Customer.API :<|> API.Action.Dashboard.Management.FRFSAlerts.API :<|> API.Action.Dashboard.Management.FRFSTicket.API :<|> API.Action.Dashboard.Management.Invoice.API :<|> API.Action.Dashboard.Management.Media.API :<|> API.Action.Dashboard.Management.Merchant.API :<|> API.Action.Dashboard.Management.NammaTag.API :<|> API.Action.Dashboard.Management.Ride.API :<|> API.Action.Dashboard.Management.Sos.API :<|> API.Action.Dashboard.Management.SosMedia.API :<|> API.Action.Dashboard.Management.System.API)

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = API.Action.Dashboard.Management.Account.handler merchantId city :<|> API.Action.Dashboard.Management.AlertIncident.handler merchantId city :<|> API.Action.Dashboard.Management.Booking.handler merchantId city :<|> API.Action.Dashboard.Management.Customer.handler merchantId city :<|> API.Action.Dashboard.Management.FRFSAlerts.handler merchantId city :<|> API.Action.Dashboard.Management.FRFSTicket.handler merchantId city :<|> API.Action.Dashboard.Management.Invoice.handler merchantId city :<|> API.Action.Dashboard.Management.Media.handler merchantId city :<|> API.Action.Dashboard.Management.Merchant.handler merchantId city :<|> API.Action.Dashboard.Management.NammaTag.handler merchantId city :<|> API.Action.Dashboard.Management.Ride.handler merchantId city :<|> API.Action.Dashboard.Management.Sos.handler merchantId city :<|> API.Action.Dashboard.Management.SosMedia.handler merchantId city :<|> API.Action.Dashboard.Management.System.handler merchantId city
