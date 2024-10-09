{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module API.Client.RiderPlatform.Management
  ( callManagementAPI,
  )
where

import qualified "rider-app" API.Action.Dashboard.Management
import qualified API.Types.RiderPlatform.Management.Booking
import qualified API.Types.RiderPlatform.Management.Invoice
import qualified API.Types.RiderPlatform.Management.Merchant
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified "lib-dashboard" Domain.Types.ServerName
import Kernel.Prelude
import qualified Kernel.Types.Beckn.City
import Servant
import qualified "lib-dashboard" Tools.Auth.Merchant
import qualified "lib-dashboard" Tools.Client

data ManagementAPIs = ManagementAPIs
  { bookingDSL :: API.Types.RiderPlatform.Management.Booking.BookingAPIs,
    merchantDSL :: API.Types.RiderPlatform.Management.Merchant.MerchantAPIs,
    invoiceDSL :: API.Types.RiderPlatform.Management.Invoice.InvoiceAPIs
  }

mkManagementAPIs :: Tools.Auth.Merchant.CheckedShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.City.City -> Text -> ManagementAPIs
mkManagementAPIs merchantId city token = do
  let bookingDSL = API.Types.RiderPlatform.Management.Booking.mkBookingAPIs bookingClientDSL
  let invoiceDSL = API.Types.RiderPlatform.Management.Invoice.mkInvoiceAPIs invoiceClientDSL
  let merchantDSL = API.Types.RiderPlatform.Management.Merchant.mkMerchantAPIs merchantClientDSL
  ManagementAPIs {..}
  where
    bookingClientDSL
      :<|> invoiceClientDSL
      :<|> merchantClientDSL =
        Tools.Client.clientWithMerchantAndCity (Proxy :: Proxy API.Action.Dashboard.Management.API) merchantId city token

-- TODO serverName=APP_BACKEND_MANAGEMENT should be configurable per folder. Then we can remove it from Auth spec
callManagementAPI ::
  forall m r b c.
  Tools.Client.DashboardClient ManagementAPIs m r b c =>
  Tools.Auth.Merchant.CheckedShortId Domain.Types.Merchant.Merchant ->
  Kernel.Types.Beckn.City.City ->
  (ManagementAPIs -> b) ->
  c
callManagementAPI merchantId city = Tools.Client.callServerAPI @_ @m @r Domain.Types.ServerName.APP_BACKEND_MANAGEMENT (mkManagementAPIs merchantId city) "callManagementAPI"
