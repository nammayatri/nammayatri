{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Client.ProviderPlatform.AppManagement where

import qualified "dynamic-offer-driver-app" API.Dashboard
import qualified "dynamic-offer-driver-app" API.Types.Dashboard.AppManagement.Driver
import qualified "dynamic-offer-driver-app" API.Types.Dashboard.AppManagement.DriverSubscription
import qualified "dynamic-offer-driver-app" API.Types.Dashboard.AppManagement.DriverWallet
import qualified "dynamic-offer-driver-app" API.Types.Dashboard.AppManagement.Overlay
import qualified "dynamic-offer-driver-app" API.Types.Dashboard.AppManagement.Penalty
import qualified "dynamic-offer-driver-app" API.Types.Dashboard.AppManagement.Subscription
import qualified "dynamic-offer-driver-app" API.Types.Dashboard.AppManagement.SubscriptionTransaction
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
    driverWalletDSL :: API.Types.Dashboard.AppManagement.DriverWallet.DriverWalletAPIs,
    overlayDSL :: API.Types.Dashboard.AppManagement.Overlay.OverlayAPIs,
    penaltyDSL :: API.Types.Dashboard.AppManagement.Penalty.PenaltyAPIs,
    subscriptionDSL :: API.Types.Dashboard.AppManagement.Subscription.SubscriptionAPIs,
    subscriptionTransactionDSL :: API.Types.Dashboard.AppManagement.SubscriptionTransaction.SubscriptionTransactionAPIs
  }

mkAppManagementAPIs :: (Tools.Auth.Merchant.CheckedShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.City.City -> Text -> AppManagementAPIs)
mkAppManagementAPIs merchantId city token = do
  let driverDSL = API.Types.Dashboard.AppManagement.Driver.mkDriverAPIs driverClientDSL
  let driverSubscriptionDSL = API.Types.Dashboard.AppManagement.DriverSubscription.mkDriverSubscriptionAPIs driverSubscriptionClientDSL
  let driverWalletDSL = API.Types.Dashboard.AppManagement.DriverWallet.mkDriverWalletAPIs driverWalletClientDSL
  let overlayDSL = API.Types.Dashboard.AppManagement.Overlay.mkOverlayAPIs overlayClientDSL
  let penaltyDSL = API.Types.Dashboard.AppManagement.Penalty.mkPenaltyAPIs penaltyClientDSL
  let subscriptionDSL = API.Types.Dashboard.AppManagement.Subscription.mkSubscriptionAPIs subscriptionClientDSL
  let subscriptionTransactionDSL = API.Types.Dashboard.AppManagement.SubscriptionTransaction.mkSubscriptionTransactionAPIs subscriptionTransactionClientDSL
  (AppManagementAPIs {..})
  where
    driverClientDSL :<|> driverSubscriptionClientDSL :<|> driverWalletClientDSL :<|> overlayClientDSL :<|> penaltyClientDSL :<|> subscriptionClientDSL :<|> subscriptionTransactionClientDSL = Tools.Client.clientWithMerchantAndCity (Proxy :: Proxy API.Dashboard.AppManagementDSLAPI) merchantId city token

callAppManagementAPI ::
  forall m r b c.
  Tools.Client.DashboardClient AppManagementAPIs m r b c =>
  (Tools.Auth.Merchant.CheckedShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.City.City -> (AppManagementAPIs -> b) -> c)
callAppManagementAPI merchantId city = Tools.Client.callServerAPI @_ @m @r Domain.Types.ServerName.DRIVER_OFFER_BPP_MANAGEMENT (mkAppManagementAPIs merchantId city) "callAppManagementAPI"
