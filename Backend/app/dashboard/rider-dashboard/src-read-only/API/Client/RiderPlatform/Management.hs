{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Client.RiderPlatform.Management where

import qualified "rider-app" API.Dashboard
import qualified API.Types.RiderPlatform.Management.AlertIncident
import qualified API.Types.RiderPlatform.Management.Booking
import qualified API.Types.RiderPlatform.Management.Customer
import qualified API.Types.RiderPlatform.Management.DeviceVehicleMapping
import qualified API.Types.RiderPlatform.Management.FRFSAlerts
import qualified API.Types.RiderPlatform.Management.FRFSTicket
import qualified API.Types.RiderPlatform.Management.Invoice
import qualified API.Types.RiderPlatform.Management.Media
import qualified API.Types.RiderPlatform.Management.Merchant
import qualified API.Types.RiderPlatform.Management.NammaTag
import qualified API.Types.RiderPlatform.Management.Ride
import qualified API.Types.RiderPlatform.Management.Sos
import qualified API.Types.RiderPlatform.Management.SosMedia
import qualified API.Types.RiderPlatform.Management.System
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified "lib-dashboard" Domain.Types.ServerName
import Kernel.Prelude
import qualified Kernel.Types.Beckn.City
import Servant
import qualified "lib-dashboard" Tools.Auth.Merchant
import qualified "lib-dashboard" Tools.Client

data ManagementAPIs = ManagementAPIs
  { alertIncidentDSL :: API.Types.RiderPlatform.Management.AlertIncident.AlertIncidentAPIs,
    bookingDSL :: API.Types.RiderPlatform.Management.Booking.BookingAPIs,
    customerDSL :: API.Types.RiderPlatform.Management.Customer.CustomerAPIs,
    deviceVehicleMappingDSL :: API.Types.RiderPlatform.Management.DeviceVehicleMapping.DeviceVehicleMappingAPIs,
    fRFSAlertsDSL :: API.Types.RiderPlatform.Management.FRFSAlerts.FRFSAlertsAPIs,
    fRFSTicketDSL :: API.Types.RiderPlatform.Management.FRFSTicket.FRFSTicketAPIs,
    invoiceDSL :: API.Types.RiderPlatform.Management.Invoice.InvoiceAPIs,
    mediaDSL :: API.Types.RiderPlatform.Management.Media.MediaAPIs,
    merchantDSL :: API.Types.RiderPlatform.Management.Merchant.MerchantAPIs,
    nammaTagDSL :: API.Types.RiderPlatform.Management.NammaTag.NammaTagAPIs,
    rideDSL :: API.Types.RiderPlatform.Management.Ride.RideAPIs,
    sosDSL :: API.Types.RiderPlatform.Management.Sos.SosAPIs,
    sosMediaDSL :: API.Types.RiderPlatform.Management.SosMedia.SosMediaAPIs,
    systemDSL :: API.Types.RiderPlatform.Management.System.SystemAPIs
  }

mkManagementAPIs :: (Tools.Auth.Merchant.CheckedShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.City.City -> Text -> ManagementAPIs)
mkManagementAPIs merchantId city token = do
  let alertIncidentDSL = API.Types.RiderPlatform.Management.AlertIncident.mkAlertIncidentAPIs alertIncidentClientDSL
  let bookingDSL = API.Types.RiderPlatform.Management.Booking.mkBookingAPIs bookingClientDSL
  let customerDSL = API.Types.RiderPlatform.Management.Customer.mkCustomerAPIs customerClientDSL
  let deviceVehicleMappingDSL = API.Types.RiderPlatform.Management.DeviceVehicleMapping.mkDeviceVehicleMappingAPIs deviceVehicleMappingClientDSL
  let fRFSAlertsDSL = API.Types.RiderPlatform.Management.FRFSAlerts.mkFRFSAlertsAPIs fRFSAlertsClientDSL
  let fRFSTicketDSL = API.Types.RiderPlatform.Management.FRFSTicket.mkFRFSTicketAPIs fRFSTicketClientDSL
  let invoiceDSL = API.Types.RiderPlatform.Management.Invoice.mkInvoiceAPIs invoiceClientDSL
  let mediaDSL = API.Types.RiderPlatform.Management.Media.mkMediaAPIs mediaClientDSL
  let merchantDSL = API.Types.RiderPlatform.Management.Merchant.mkMerchantAPIs merchantClientDSL
  let nammaTagDSL = API.Types.RiderPlatform.Management.NammaTag.mkNammaTagAPIs nammaTagClientDSL
  let rideDSL = API.Types.RiderPlatform.Management.Ride.mkRideAPIs rideClientDSL
  let sosDSL = API.Types.RiderPlatform.Management.Sos.mkSosAPIs sosClientDSL
  let sosMediaDSL = API.Types.RiderPlatform.Management.SosMedia.mkSosMediaAPIs sosMediaClientDSL
  let systemDSL = API.Types.RiderPlatform.Management.System.mkSystemAPIs systemClientDSL
  (ManagementAPIs {..})
  where
    alertIncidentClientDSL :<|> bookingClientDSL :<|> customerClientDSL :<|> deviceVehicleMappingClientDSL :<|> fRFSAlertsClientDSL :<|> fRFSTicketClientDSL :<|> invoiceClientDSL :<|> mediaClientDSL :<|> merchantClientDSL :<|> nammaTagClientDSL :<|> rideClientDSL :<|> sosClientDSL :<|> sosMediaClientDSL :<|> systemClientDSL = Tools.Client.clientWithMerchantAndCity (Proxy :: Proxy API.Dashboard.ManagementDSLAPI) merchantId city token

callManagementAPI ::
  forall m r b c.
  Tools.Client.DashboardClient ManagementAPIs m r b c =>
  (Tools.Auth.Merchant.CheckedShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.City.City -> (ManagementAPIs -> b) -> c)
callManagementAPI merchantId city = Tools.Client.callServerAPI @_ @m @r Domain.Types.ServerName.APP_BACKEND_MANAGEMENT (mkManagementAPIs merchantId city) "callManagementAPI"
