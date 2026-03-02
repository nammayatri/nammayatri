{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Client.RiderPlatform.AppManagement where

import qualified "rider-app" API.Dashboard
import qualified "rider-app" API.Types.Dashboard.AppManagement.Customer
import qualified "rider-app" API.Types.Dashboard.AppManagement.EventManagement
import qualified "rider-app" API.Types.Dashboard.AppManagement.MerchantOnboarding
import qualified "rider-app" API.Types.Dashboard.AppManagement.Pass
import qualified "rider-app" API.Types.Dashboard.AppManagement.Payment
import qualified "rider-app" API.Types.Dashboard.AppManagement.TicketDashboard
import qualified "rider-app" API.Types.Dashboard.AppManagement.Tickets
import qualified "rider-app" API.Types.Dashboard.AppManagement.TransitOperator
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified "lib-dashboard" Domain.Types.ServerName
import Kernel.Prelude
import qualified Kernel.Types.Beckn.City
import Servant
import qualified "lib-dashboard" Tools.Auth.Merchant
import qualified "lib-dashboard" Tools.Client

data AppManagementAPIs = AppManagementAPIs
  { customerDSL :: API.Types.Dashboard.AppManagement.Customer.CustomerAPIs,
    eventManagementDSL :: API.Types.Dashboard.AppManagement.EventManagement.EventManagementAPIs,
    merchantOnboardingDSL :: API.Types.Dashboard.AppManagement.MerchantOnboarding.MerchantOnboardingAPIs,
    passDSL :: API.Types.Dashboard.AppManagement.Pass.PassAPIs,
    paymentDSL :: API.Types.Dashboard.AppManagement.Payment.PaymentAPIs,
    ticketDashboardDSL :: API.Types.Dashboard.AppManagement.TicketDashboard.TicketDashboardAPIs,
    ticketsDSL :: API.Types.Dashboard.AppManagement.Tickets.TicketsAPIs,
    transitOperatorDSL :: API.Types.Dashboard.AppManagement.TransitOperator.TransitOperatorAPIs
  }

mkAppManagementAPIs :: (Tools.Auth.Merchant.CheckedShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.City.City -> Text -> AppManagementAPIs)
mkAppManagementAPIs merchantId city token = do
  let customerDSL = API.Types.Dashboard.AppManagement.Customer.mkCustomerAPIs customerClientDSL
  let eventManagementDSL = API.Types.Dashboard.AppManagement.EventManagement.mkEventManagementAPIs eventManagementClientDSL
  let merchantOnboardingDSL = API.Types.Dashboard.AppManagement.MerchantOnboarding.mkMerchantOnboardingAPIs merchantOnboardingClientDSL
  let passDSL = API.Types.Dashboard.AppManagement.Pass.mkPassAPIs passClientDSL
  let paymentDSL = API.Types.Dashboard.AppManagement.Payment.mkPaymentAPIs paymentClientDSL
  let ticketDashboardDSL = API.Types.Dashboard.AppManagement.TicketDashboard.mkTicketDashboardAPIs ticketDashboardClientDSL
  let ticketsDSL = API.Types.Dashboard.AppManagement.Tickets.mkTicketsAPIs ticketsClientDSL
  let transitOperatorDSL = API.Types.Dashboard.AppManagement.TransitOperator.mkTransitOperatorAPIs transitOperatorClientDSL
  (AppManagementAPIs {..})
  where
    customerClientDSL :<|> eventManagementClientDSL :<|> merchantOnboardingClientDSL :<|> passClientDSL :<|> paymentClientDSL :<|> ticketDashboardClientDSL :<|> ticketsClientDSL :<|> transitOperatorClientDSL = Tools.Client.clientWithMerchantAndCity (Proxy :: Proxy API.Dashboard.AppManagementDSLAPI) merchantId city token

callAppManagementAPI ::
  forall m r b c.
  Tools.Client.DashboardClient AppManagementAPIs m r b c =>
  (Tools.Auth.Merchant.CheckedShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.City.City -> (AppManagementAPIs -> b) -> c)
callAppManagementAPI merchantId city = Tools.Client.callServerAPI @_ @m @r Domain.Types.ServerName.APP_BACKEND_MANAGEMENT (mkAppManagementAPIs merchantId city) "callAppManagementAPI"
