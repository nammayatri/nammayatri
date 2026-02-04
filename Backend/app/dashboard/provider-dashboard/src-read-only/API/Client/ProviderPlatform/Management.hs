{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Client.ProviderPlatform.Management where

import qualified "dynamic-offer-driver-app" API.Dashboard
import qualified API.Types.ProviderPlatform.Management.Account
import qualified API.Types.ProviderPlatform.Management.Booking
import qualified API.Types.ProviderPlatform.Management.CoinsConfig
import qualified API.Types.ProviderPlatform.Management.Driver
import qualified API.Types.ProviderPlatform.Management.DriverCoins
import qualified API.Types.ProviderPlatform.Management.DriverGoHome
import qualified API.Types.ProviderPlatform.Management.DriverReferral
import qualified API.Types.ProviderPlatform.Management.DriverRegistration
import qualified API.Types.ProviderPlatform.Management.EntityInfo
import qualified API.Types.ProviderPlatform.Management.Exophone
import qualified API.Types.ProviderPlatform.Management.Media
import qualified API.Types.ProviderPlatform.Management.MediaFileDocument
import qualified API.Types.ProviderPlatform.Management.Merchant
import qualified API.Types.ProviderPlatform.Management.Message
import qualified API.Types.ProviderPlatform.Management.NammaTag
import qualified API.Types.ProviderPlatform.Management.Payout
import qualified API.Types.ProviderPlatform.Management.Revenue
import qualified API.Types.ProviderPlatform.Management.Ride
import qualified API.Types.ProviderPlatform.Management.System
import qualified API.Types.ProviderPlatform.Management.VehicleInfo
import qualified API.Types.ProviderPlatform.Management.Volunteer
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified "lib-dashboard" Domain.Types.ServerName
import Kernel.Prelude
import qualified Kernel.Types.Beckn.City
import Servant
import qualified "lib-dashboard" Tools.Auth.Merchant
import qualified "lib-dashboard" Tools.Client

data ManagementAPIs = ManagementAPIs
  { accountDSL :: API.Types.ProviderPlatform.Management.Account.AccountAPIs,
    bookingDSL :: API.Types.ProviderPlatform.Management.Booking.BookingAPIs,
    coinsConfigDSL :: API.Types.ProviderPlatform.Management.CoinsConfig.CoinsConfigAPIs,
    driverDSL :: API.Types.ProviderPlatform.Management.Driver.DriverAPIs,
    driverCoinsDSL :: API.Types.ProviderPlatform.Management.DriverCoins.DriverCoinsAPIs,
    driverGoHomeDSL :: API.Types.ProviderPlatform.Management.DriverGoHome.DriverGoHomeAPIs,
    driverReferralDSL :: API.Types.ProviderPlatform.Management.DriverReferral.DriverReferralAPIs,
    driverRegistrationDSL :: API.Types.ProviderPlatform.Management.DriverRegistration.DriverRegistrationAPIs,
    entityInfoDSL :: API.Types.ProviderPlatform.Management.EntityInfo.EntityInfoAPIs,
    exophoneDSL :: API.Types.ProviderPlatform.Management.Exophone.ExophoneAPIs,
    mediaDSL :: API.Types.ProviderPlatform.Management.Media.MediaAPIs,
    mediaFileDocumentDSL :: API.Types.ProviderPlatform.Management.MediaFileDocument.MediaFileDocumentAPIs,
    merchantDSL :: API.Types.ProviderPlatform.Management.Merchant.MerchantAPIs,
    messageDSL :: API.Types.ProviderPlatform.Management.Message.MessageAPIs,
    nammaTagDSL :: API.Types.ProviderPlatform.Management.NammaTag.NammaTagAPIs,
    payoutDSL :: API.Types.ProviderPlatform.Management.Payout.PayoutAPIs,
    revenueDSL :: API.Types.ProviderPlatform.Management.Revenue.RevenueAPIs,
    rideDSL :: API.Types.ProviderPlatform.Management.Ride.RideAPIs,
    systemDSL :: API.Types.ProviderPlatform.Management.System.SystemAPIs,
    vehicleInfoDSL :: API.Types.ProviderPlatform.Management.VehicleInfo.VehicleInfoAPIs,
    volunteerDSL :: API.Types.ProviderPlatform.Management.Volunteer.VolunteerAPIs
  }

mkManagementAPIs :: (Tools.Auth.Merchant.CheckedShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.City.City -> Text -> ManagementAPIs)
mkManagementAPIs merchantId city token = do
  let accountDSL = API.Types.ProviderPlatform.Management.Account.mkAccountAPIs accountClientDSL
  let bookingDSL = API.Types.ProviderPlatform.Management.Booking.mkBookingAPIs bookingClientDSL
  let coinsConfigDSL = API.Types.ProviderPlatform.Management.CoinsConfig.mkCoinsConfigAPIs coinsConfigClientDSL
  let driverDSL = API.Types.ProviderPlatform.Management.Driver.mkDriverAPIs driverClientDSL
  let driverCoinsDSL = API.Types.ProviderPlatform.Management.DriverCoins.mkDriverCoinsAPIs driverCoinsClientDSL
  let driverGoHomeDSL = API.Types.ProviderPlatform.Management.DriverGoHome.mkDriverGoHomeAPIs driverGoHomeClientDSL
  let driverReferralDSL = API.Types.ProviderPlatform.Management.DriverReferral.mkDriverReferralAPIs driverReferralClientDSL
  let driverRegistrationDSL = API.Types.ProviderPlatform.Management.DriverRegistration.mkDriverRegistrationAPIs driverRegistrationClientDSL
  let entityInfoDSL = API.Types.ProviderPlatform.Management.EntityInfo.mkEntityInfoAPIs entityInfoClientDSL
  let exophoneDSL = API.Types.ProviderPlatform.Management.Exophone.mkExophoneAPIs exophoneClientDSL
  let mediaDSL = API.Types.ProviderPlatform.Management.Media.mkMediaAPIs mediaClientDSL
  let mediaFileDocumentDSL = API.Types.ProviderPlatform.Management.MediaFileDocument.mkMediaFileDocumentAPIs mediaFileDocumentClientDSL
  let merchantDSL = API.Types.ProviderPlatform.Management.Merchant.mkMerchantAPIs merchantClientDSL
  let messageDSL = API.Types.ProviderPlatform.Management.Message.mkMessageAPIs messageClientDSL
  let nammaTagDSL = API.Types.ProviderPlatform.Management.NammaTag.mkNammaTagAPIs nammaTagClientDSL
  let payoutDSL = API.Types.ProviderPlatform.Management.Payout.mkPayoutAPIs payoutClientDSL
  let revenueDSL = API.Types.ProviderPlatform.Management.Revenue.mkRevenueAPIs revenueClientDSL
  let rideDSL = API.Types.ProviderPlatform.Management.Ride.mkRideAPIs rideClientDSL
  let systemDSL = API.Types.ProviderPlatform.Management.System.mkSystemAPIs systemClientDSL
  let vehicleInfoDSL = API.Types.ProviderPlatform.Management.VehicleInfo.mkVehicleInfoAPIs vehicleInfoClientDSL
  let volunteerDSL = API.Types.ProviderPlatform.Management.Volunteer.mkVolunteerAPIs volunteerClientDSL
  (ManagementAPIs {..})
  where
    accountClientDSL :<|> bookingClientDSL :<|> coinsConfigClientDSL :<|> driverClientDSL :<|> driverCoinsClientDSL :<|> driverGoHomeClientDSL :<|> driverReferralClientDSL :<|> driverRegistrationClientDSL :<|> entityInfoClientDSL :<|> mediaClientDSL :<|> mediaFileDocumentClientDSL :<|> merchantClientDSL :<|> messageClientDSL :<|> nammaTagClientDSL :<|> payoutClientDSL :<|> revenueClientDSL :<|> rideClientDSL :<|> systemClientDSL :<|> vehicleInfoClientDSL :<|> volunteerClientDSL = Tools.Client.clientWithMerchantAndCity (Proxy :: Proxy API.Dashboard.ManagementDSLAPI) merchantId city token
    accountClientDSL :<|> bookingClientDSL :<|> coinsConfigClientDSL :<|> driverClientDSL :<|> driverCoinsClientDSL :<|> driverGoHomeClientDSL :<|> driverReferralClientDSL :<|> driverRegistrationClientDSL :<|> exophoneClientDSL :<|> mediaClientDSL :<|> mediaFileDocumentClientDSL :<|> merchantClientDSL :<|> messageClientDSL :<|> nammaTagClientDSL :<|> payoutClientDSL :<|> revenueClientDSL :<|> rideClientDSL :<|> systemClientDSL :<|> vehicleInfoClientDSL :<|> volunteerClientDSL = Tools.Client.clientWithMerchantAndCity (Proxy :: Proxy API.Dashboard.ManagementDSLAPI) merchantId city token

callManagementAPI ::
  forall m r b c.
  Tools.Client.DashboardClient ManagementAPIs m r b c =>
  (Tools.Auth.Merchant.CheckedShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.City.City -> (ManagementAPIs -> b) -> c)
callManagementAPI merchantId city = Tools.Client.callServerAPI @_ @m @r Domain.Types.ServerName.DRIVER_OFFER_BPP_MANAGEMENT (mkManagementAPIs merchantId city) "callManagementAPI"
