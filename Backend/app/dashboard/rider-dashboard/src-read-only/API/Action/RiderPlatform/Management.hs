{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.RiderPlatform.Management where

import qualified API.Action.RiderPlatform.Management.Booking
import qualified API.Action.RiderPlatform.Management.Customer
import qualified API.Action.RiderPlatform.Management.FRFSTicket
import qualified API.Action.RiderPlatform.Management.Invoice
import qualified API.Action.RiderPlatform.Management.Media
import qualified API.Action.RiderPlatform.Management.Merchant
import qualified API.Action.RiderPlatform.Management.NammaTag
import qualified API.Action.RiderPlatform.Management.Ride
import qualified API.Action.RiderPlatform.Management.Sos
import qualified API.Action.RiderPlatform.Management.SosMedia
import qualified API.Action.RiderPlatform.Management.System
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified "lib-dashboard" Environment
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Servant

type API = (API.Action.RiderPlatform.Management.Booking.API :<|> API.Action.RiderPlatform.Management.Customer.API :<|> API.Action.RiderPlatform.Management.FRFSTicket.API :<|> API.Action.RiderPlatform.Management.Invoice.API :<|> API.Action.RiderPlatform.Management.Media.API :<|> API.Action.RiderPlatform.Management.Merchant.API :<|> API.Action.RiderPlatform.Management.NammaTag.API :<|> API.Action.RiderPlatform.Management.Ride.API :<|> API.Action.RiderPlatform.Management.Sos.API :<|> API.Action.RiderPlatform.Management.SosMedia.API :<|> API.Action.RiderPlatform.Management.System.API)

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = API.Action.RiderPlatform.Management.Booking.handler merchantId city :<|> API.Action.RiderPlatform.Management.Customer.handler merchantId city :<|> API.Action.RiderPlatform.Management.FRFSTicket.handler merchantId city :<|> API.Action.RiderPlatform.Management.Invoice.handler merchantId city :<|> API.Action.RiderPlatform.Management.Media.handler merchantId city :<|> API.Action.RiderPlatform.Management.Merchant.handler merchantId city :<|> API.Action.RiderPlatform.Management.NammaTag.handler merchantId city :<|> API.Action.RiderPlatform.Management.Ride.handler merchantId city :<|> API.Action.RiderPlatform.Management.Sos.handler merchantId city :<|> API.Action.RiderPlatform.Management.SosMedia.handler merchantId city :<|> API.Action.RiderPlatform.Management.System.handler merchantId city
