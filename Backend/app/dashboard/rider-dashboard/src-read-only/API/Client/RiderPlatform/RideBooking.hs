{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Client.RiderPlatform.RideBooking where

import qualified "rider-app" API.Dashboard
import qualified "rider-app" API.Types.Dashboard.RideBooking.Booking
import qualified "rider-app" API.Types.Dashboard.RideBooking.Cancel
import qualified "rider-app" API.Types.Dashboard.RideBooking.Confirm
import qualified "rider-app" API.Types.Dashboard.RideBooking.Frontend
import qualified "rider-app" API.Types.Dashboard.RideBooking.Maps
import qualified "rider-app" API.Types.Dashboard.RideBooking.MultiModal
import qualified "rider-app" API.Types.Dashboard.RideBooking.NotifyRideInfo
import qualified "rider-app" API.Types.Dashboard.RideBooking.Profile
import qualified "rider-app" API.Types.Dashboard.RideBooking.Quote
import qualified "rider-app" API.Types.Dashboard.RideBooking.Registration
import qualified "rider-app" API.Types.Dashboard.RideBooking.Search
import qualified "rider-app" API.Types.Dashboard.RideBooking.Select
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified "lib-dashboard" Domain.Types.ServerName
import Kernel.Prelude
import qualified Kernel.Types.Beckn.City
import Servant
import qualified "lib-dashboard" Tools.Auth.Merchant
import qualified "lib-dashboard" Tools.Client

data RideBookingAPIs = RideBookingAPIs
  { bookingDSL :: API.Types.Dashboard.RideBooking.Booking.BookingAPIs,
    cancelDSL :: API.Types.Dashboard.RideBooking.Cancel.CancelAPIs,
    confirmDSL :: API.Types.Dashboard.RideBooking.Confirm.ConfirmAPIs,
    frontendDSL :: API.Types.Dashboard.RideBooking.Frontend.FrontendAPIs,
    mapsDSL :: API.Types.Dashboard.RideBooking.Maps.MapsAPIs,
    multiModalDSL :: API.Types.Dashboard.RideBooking.MultiModal.MultiModalAPIs,
    notifyRideInfoDSL :: API.Types.Dashboard.RideBooking.NotifyRideInfo.NotifyRideInfoAPIs,
    profileDSL :: API.Types.Dashboard.RideBooking.Profile.ProfileAPIs,
    quoteDSL :: API.Types.Dashboard.RideBooking.Quote.QuoteAPIs,
    registrationDSL :: API.Types.Dashboard.RideBooking.Registration.RegistrationAPIs,
    searchDSL :: API.Types.Dashboard.RideBooking.Search.SearchAPIs,
    selectDSL :: API.Types.Dashboard.RideBooking.Select.SelectAPIs
  }

mkRideBookingAPIs :: (Tools.Auth.Merchant.CheckedShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.City.City -> Text -> RideBookingAPIs)
mkRideBookingAPIs merchantId city token = do
  let bookingDSL = API.Types.Dashboard.RideBooking.Booking.mkBookingAPIs bookingClientDSL
  let cancelDSL = API.Types.Dashboard.RideBooking.Cancel.mkCancelAPIs cancelClientDSL
  let confirmDSL = API.Types.Dashboard.RideBooking.Confirm.mkConfirmAPIs confirmClientDSL
  let frontendDSL = API.Types.Dashboard.RideBooking.Frontend.mkFrontendAPIs frontendClientDSL
  let mapsDSL = API.Types.Dashboard.RideBooking.Maps.mkMapsAPIs mapsClientDSL
  let multiModalDSL = API.Types.Dashboard.RideBooking.MultiModal.mkMultiModalAPIs multiModalClientDSL
  let notifyRideInfoDSL = API.Types.Dashboard.RideBooking.NotifyRideInfo.mkNotifyRideInfoAPIs notifyRideInfoClientDSL
  let profileDSL = API.Types.Dashboard.RideBooking.Profile.mkProfileAPIs profileClientDSL
  let quoteDSL = API.Types.Dashboard.RideBooking.Quote.mkQuoteAPIs quoteClientDSL
  let registrationDSL = API.Types.Dashboard.RideBooking.Registration.mkRegistrationAPIs registrationClientDSL
  let searchDSL = API.Types.Dashboard.RideBooking.Search.mkSearchAPIs searchClientDSL
  let selectDSL = API.Types.Dashboard.RideBooking.Select.mkSelectAPIs selectClientDSL
  (RideBookingAPIs {..})
  where
    bookingClientDSL :<|> cancelClientDSL :<|> confirmClientDSL :<|> frontendClientDSL :<|> mapsClientDSL :<|> multiModalClientDSL :<|> notifyRideInfoClientDSL :<|> profileClientDSL :<|> quoteClientDSL :<|> registrationClientDSL :<|> searchClientDSL :<|> selectClientDSL = Tools.Client.clientWithMerchantAndCity (Proxy :: Proxy API.Dashboard.RideBookingDSLAPI) merchantId city token

callRideBookingAPI ::
  forall m r b c.
  Tools.Client.DashboardClient RideBookingAPIs m r b c =>
  (Tools.Auth.Merchant.CheckedShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.City.City -> (RideBookingAPIs -> b) -> c)
callRideBookingAPI merchantId city = Tools.Client.callServerAPI @_ @m @r Domain.Types.ServerName.APP_BACKEND (mkRideBookingAPIs merchantId city) "callRideBookingAPI"
