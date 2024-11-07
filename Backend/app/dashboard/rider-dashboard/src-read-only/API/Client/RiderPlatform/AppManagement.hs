{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Client.RiderPlatform.AppManagement where

import qualified "rider-app" API.Dashboard
import qualified "rider-app" API.Types.Dashboard.AppManagement.HotSpot
import qualified "rider-app" API.Types.Dashboard.AppManagement.Tickets
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified "lib-dashboard" Domain.Types.ServerName
import Kernel.Prelude
import qualified Kernel.Types.Beckn.City
import Servant
import qualified "lib-dashboard" Tools.Auth.Merchant
import qualified "lib-dashboard" Tools.Client

data AppManagementAPIs = AppManagementAPIs {hotSpotDSL :: API.Types.Dashboard.AppManagement.HotSpot.HotSpotAPIs, ticketsDSL :: API.Types.Dashboard.AppManagement.Tickets.TicketsAPIs}

mkAppManagementAPIs :: (Tools.Auth.Merchant.CheckedShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.City.City -> Text -> AppManagementAPIs)
mkAppManagementAPIs merchantId city token = do
  let hotSpotDSL = API.Types.Dashboard.AppManagement.HotSpot.mkHotSpotAPIs hotSpotClientDSL
  let ticketsDSL = API.Types.Dashboard.AppManagement.Tickets.mkTicketsAPIs ticketsClientDSL
  (AppManagementAPIs {..})
  where
    hotSpotClientDSL :<|> ticketsClientDSL = Tools.Client.clientWithMerchantAndCity (Proxy :: Proxy API.Dashboard.AppManagementDSLAPI) merchantId city token

callAppManagementAPI ::
  forall m r b c.
  Tools.Client.DashboardClient AppManagementAPIs m r b c =>
  (Tools.Auth.Merchant.CheckedShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.City.City -> (AppManagementAPIs -> b) -> c)
callAppManagementAPI merchantId city = Tools.Client.callServerAPI @_ @m @r Domain.Types.ServerName.APP_BACKEND_MANAGEMENT (mkAppManagementAPIs merchantId city) "callAppManagementAPI"
