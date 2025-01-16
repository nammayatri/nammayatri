{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Client.ProviderPlatform.Management where

import qualified "dynamic-offer-driver-app" API.Dashboard
import qualified API.Types.ProviderPlatform.Management.Booking
import qualified API.Types.ProviderPlatform.Management.Driver
import qualified API.Types.ProviderPlatform.Management.DriverCoins
import qualified API.Types.ProviderPlatform.Management.DriverGoHome
import qualified API.Types.ProviderPlatform.Management.DriverReferral
import qualified API.Types.ProviderPlatform.Management.DriverRegistration
import qualified API.Types.ProviderPlatform.Management.Media
import qualified API.Types.ProviderPlatform.Management.Merchant
import qualified API.Types.ProviderPlatform.Management.Message
import qualified API.Types.ProviderPlatform.Management.NammaTag
import qualified API.Types.ProviderPlatform.Management.Payout
import qualified API.Types.ProviderPlatform.Management.Revenue
import qualified API.Types.ProviderPlatform.Management.Ride
import qualified API.Types.ProviderPlatform.Management.System
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified "lib-dashboard" Domain.Types.ServerName
import Kernel.Prelude
import qualified Kernel.Types.Beckn.City
import Servant
import qualified "lib-dashboard" Tools.Auth.Merchant
import qualified "lib-dashboard" Tools.Client

data ManagementAPIs = ManagementAPIs
  { bookingDSL :: API.Types.ProviderPlatform.Management.Booking.BookingAPIs,
    driverDSL :: API.Types.ProviderPlatform.Management.Driver.DriverAPIs,
    driverCoinsDSL :: API.Types.ProviderPlatform.Management.DriverCoins.DriverCoinsAPIs,
    driverGoHomeDSL :: API.Types.ProviderPlatform.Management.DriverGoHome.DriverGoHomeAPIs,
    driverReferralDSL :: API.Types.ProviderPlatform.Management.DriverReferral.DriverReferralAPIs,
    driverRegistrationDSL :: API.Types.ProviderPlatform.Management.DriverRegistration.DriverRegistrationAPIs,
    mediaDSL :: API.Types.ProviderPlatform.Management.Media.MediaAPIs,
    merchantDSL :: API.Types.ProviderPlatform.Management.Merchant.MerchantAPIs,
    messageDSL :: API.Types.ProviderPlatform.Management.Message.MessageAPIs,
    nammaTagDSL :: API.Types.ProviderPlatform.Management.NammaTag.NammaTagAPIs,
    payoutDSL :: API.Types.ProviderPlatform.Management.Payout.PayoutAPIs,
    revenueDSL :: API.Types.ProviderPlatform.Management.Revenue.RevenueAPIs,
    rideDSL :: API.Types.ProviderPlatform.Management.Ride.RideAPIs,
    systemDSL :: API.Types.ProviderPlatform.Management.System.SystemAPIs
  }

mkManagementAPIs :: (Tools.Auth.Merchant.CheckedShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.City.City -> Text -> ManagementAPIs)
mkManagementAPIs merchantId city token = do
  let bookingDSL = API.Types.ProviderPlatform.Management.Booking.mkBookingAPIs bookingClientDSL
  let driverDSL = API.Types.ProviderPlatform.Management.Driver.mkDriverAPIs driverClientDSL
  let driverCoinsDSL = API.Types.ProviderPlatform.Management.DriverCoins.mkDriverCoinsAPIs driverCoinsClientDSL
  let driverGoHomeDSL = API.Types.ProviderPlatform.Management.DriverGoHome.mkDriverGoHomeAPIs driverGoHomeClientDSL
  let driverReferralDSL = API.Types.ProviderPlatform.Management.DriverReferral.mkDriverReferralAPIs driverReferralClientDSL
  let driverRegistrationDSL = API.Types.ProviderPlatform.Management.DriverRegistration.mkDriverRegistrationAPIs driverRegistrationClientDSL
  let mediaDSL = API.Types.ProviderPlatform.Management.Media.mkMediaAPIs mediaClientDSL
  let merchantDSL = API.Types.ProviderPlatform.Management.Merchant.mkMerchantAPIs merchantClientDSL
  let messageDSL = API.Types.ProviderPlatform.Management.Message.mkMessageAPIs messageClientDSL
  let nammaTagDSL = API.Types.ProviderPlatform.Management.NammaTag.mkNammaTagAPIs nammaTagClientDSL
  let payoutDSL = API.Types.ProviderPlatform.Management.Payout.mkPayoutAPIs payoutClientDSL
  let revenueDSL = API.Types.ProviderPlatform.Management.Revenue.mkRevenueAPIs revenueClientDSL
  let rideDSL = API.Types.ProviderPlatform.Management.Ride.mkRideAPIs rideClientDSL
  let systemDSL = API.Types.ProviderPlatform.Management.System.mkSystemAPIs systemClientDSL
  (ManagementAPIs {..})
  where
    bookingClientDSL :<|> driverClientDSL :<|> driverCoinsClientDSL :<|> driverGoHomeClientDSL :<|> driverReferralClientDSL :<|> driverRegistrationClientDSL :<|> mediaClientDSL :<|> merchantClientDSL :<|> messageClientDSL :<|> nammaTagClientDSL :<|> payoutClientDSL :<|> revenueClientDSL :<|> rideClientDSL :<|> systemClientDSL = Tools.Client.clientWithMerchantAndCity (Proxy :: Proxy API.Dashboard.ManagementDSLAPI) merchantId city token

callManagementAPI ::
  forall m r b c.
  Tools.Client.DashboardClient ManagementAPIs m r b c =>
  (Tools.Auth.Merchant.CheckedShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.City.City -> (ManagementAPIs -> b) -> c)
callManagementAPI merchantId city = Tools.Client.callServerAPI @_ @m @r Domain.Types.ServerName.DRIVER_OFFER_BPP_MANAGEMENT (mkManagementAPIs merchantId city) "callManagementAPI"
