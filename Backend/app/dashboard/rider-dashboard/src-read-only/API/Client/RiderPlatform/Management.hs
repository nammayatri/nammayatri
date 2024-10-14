{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Client.RiderPlatform.Management where

import qualified "rider-app" API.Dashboard
import qualified API.Types.RiderPlatform.Management.Booking
import qualified API.Types.RiderPlatform.Management.FRFSTicket
import qualified API.Types.RiderPlatform.Management.Invoice
import qualified API.Types.RiderPlatform.Management.Merchant
import qualified API.Types.RiderPlatform.Management.NammaTag
import qualified API.Types.RiderPlatform.Management.System
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified "lib-dashboard" Domain.Types.ServerName
import Kernel.Prelude
import qualified Kernel.Types.Beckn.City
import Servant
import qualified "lib-dashboard" Tools.Auth.Merchant
import qualified "lib-dashboard" Tools.Client

data ManagementAPIs = ManagementAPIs
  { bookingDSL :: API.Types.RiderPlatform.Management.Booking.BookingAPIs,
    fRFSTicketDSL :: API.Types.RiderPlatform.Management.FRFSTicket.FRFSTicketAPIs,
    invoiceDSL :: API.Types.RiderPlatform.Management.Invoice.InvoiceAPIs,
    merchantDSL :: API.Types.RiderPlatform.Management.Merchant.MerchantAPIs,
    nammaTagDSL :: API.Types.RiderPlatform.Management.NammaTag.NammaTagAPIs,
    systemDSL :: API.Types.RiderPlatform.Management.System.SystemAPIs
  }

mkManagementAPIs :: (Tools.Auth.Merchant.CheckedShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.City.City -> Text -> ManagementAPIs)
mkManagementAPIs merchantId city token = do
  let bookingDSL = API.Types.RiderPlatform.Management.Booking.mkBookingAPIs bookingClientDSL
  let fRFSTicketDSL = API.Types.RiderPlatform.Management.FRFSTicket.mkFRFSTicketAPIs fRFSTicketClientDSL
  let invoiceDSL = API.Types.RiderPlatform.Management.Invoice.mkInvoiceAPIs invoiceClientDSL
  let merchantDSL = API.Types.RiderPlatform.Management.Merchant.mkMerchantAPIs merchantClientDSL
  let nammaTagDSL = API.Types.RiderPlatform.Management.NammaTag.mkNammaTagAPIs nammaTagClientDSL
  let systemDSL = API.Types.RiderPlatform.Management.System.mkSystemAPIs systemClientDSL
  (ManagementAPIs {..})
  where
    bookingClientDSL :<|> fRFSTicketClientDSL :<|> invoiceClientDSL :<|> merchantClientDSL :<|> nammaTagClientDSL :<|> systemClientDSL = Tools.Client.clientWithMerchantAndCity (Proxy :: Proxy API.Dashboard.ManagementDSLAPI) merchantId city token

callManagementAPI ::
  forall m r b c.
  Tools.Client.DashboardClient ManagementAPIs m r b c =>
  (Tools.Auth.Merchant.CheckedShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.City.City -> (ManagementAPIs -> b) -> c)
callManagementAPI merchantId city = Tools.Client.callServerAPI @_ @m @r Domain.Types.ServerName.APP_BACKEND_MANAGEMENT (mkManagementAPIs merchantId city) "callManagementAPI"
