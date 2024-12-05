{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Client.ProviderPlatform.AppManagement where

import qualified "dynamic-offer-driver-app" API.Dashboard
import qualified "dynamic-offer-driver-app" API.Types.Dashboard.AppManagement.Driver
import qualified "dynamic-offer-driver-app" API.Types.Dashboard.AppManagement.DriverSubscription
import qualified "dynamic-offer-driver-app" API.Types.Dashboard.AppManagement.Overlay
import qualified "dynamic-offer-driver-app" API.Types.Dashboard.AppManagement.Subscription
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified "lib-dashboard" Domain.Types.ServerName
import Kernel.Prelude
import qualified Kernel.Types.Beckn.City
import Servant
import qualified "lib-dashboard" Tools.Auth.Merchant
import qualified "lib-dashboard" Tools.Client

data AppManagementAPIs = AppManagementAPIs
  { driverDSL :: API.Types.Dashboard.AppManagement.Driver.DriverAPIs,
    driverSubscriptionDSL :: API.Types.Dashboard.AppManagement.DriverSubscription.DriverSubscriptionAPIs,
    overlayDSL :: API.Types.Dashboard.AppManagement.Overlay.OverlayAPIs,
    subscriptionDSL :: API.Types.Dashboard.AppManagement.Subscription.SubscriptionAPIs
  }

mkAppManagementAPIs :: (Tools.Auth.Merchant.CheckedShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.City.City -> Text -> AppManagementAPIs)
mkAppManagementAPIs merchantId city token = do
  let driverDSL = API.Types.Dashboard.AppManagement.Driver.mkDriverAPIs driverClientDSL
  let driverSubscriptionDSL = API.Types.Dashboard.AppManagement.DriverSubscription.mkDriverSubscriptionAPIs driverSubscriptionClientDSL
  let overlayDSL = API.Types.Dashboard.AppManagement.Overlay.mkOverlayAPIs overlayClientDSL
  let subscriptionDSL = API.Types.Dashboard.AppManagement.Subscription.mkSubscriptionAPIs subscriptionClientDSL
  (AppManagementAPIs {..})
  where
    driverClientDSL :<|> driverSubscriptionClientDSL :<|> overlayClientDSL :<|> subscriptionClientDSL = Tools.Client.clientWithMerchantAndCity (Proxy :: Proxy API.Dashboard.AppManagementDSLAPI) merchantId city token

callAppManagementAPI ::
  forall m r b c.
  Tools.Client.DashboardClient AppManagementAPIs m r b c =>
  (Tools.Auth.Merchant.CheckedShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.City.City -> (AppManagementAPIs -> b) -> c)
callAppManagementAPI merchantId city = Tools.Client.callServerAPI @_ @m @r Domain.Types.ServerName.DRIVER_OFFER_BPP_MANAGEMENT (mkAppManagementAPIs merchantId city) "callAppManagementAPI"
