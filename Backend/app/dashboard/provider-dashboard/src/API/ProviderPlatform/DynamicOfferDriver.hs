{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.ProviderPlatform.DynamicOfferDriver
  ( API,
    APIV2,
    CacAPI,
    handler,
    handlerV2,
    handlerV3,
  )
where

import qualified API.Action.ProviderPlatform.Fleet.Driver as FleetDriverDSL
import qualified API.Action.ProviderPlatform.Management.Booking as ManagementBookingDSL
import qualified API.Action.ProviderPlatform.Management.Driver as ManagementDriverDSL
import qualified API.Action.ProviderPlatform.Management.DriverCoins as ManagementDriverCoinsDSL
import qualified API.Action.ProviderPlatform.Management.DriverGoHome as ManagementDriverGoHomeDSL
import qualified API.Action.ProviderPlatform.Management.DriverReferral as ManagementDriverReferralDSL
import qualified API.Action.ProviderPlatform.Management.DriverRegistration as ManagementDriverRegistrationDSL
import qualified API.Action.ProviderPlatform.Management.Merchant as ManagementMerchantDSL
import qualified API.Action.ProviderPlatform.Management.Message as ManagementMessageDSL
import qualified API.Action.ProviderPlatform.Management.NammaTag as NammaTagDSL
import qualified API.Action.ProviderPlatform.Management.Revenue as ManagementRevenueDSL
import qualified API.Action.ProviderPlatform.Management.Ride as ManagementRideDSL
import qualified API.Action.ProviderPlatform.RideBooking.Driver as RideBookingDriverDSL
import qualified API.ProviderPlatform.DynamicOfferDriver.CacAuth as CacAuth
import qualified API.ProviderPlatform.DynamicOfferDriver.Driver as Driver
import qualified API.ProviderPlatform.DynamicOfferDriver.Driver.Registration as DriverRegistration
import qualified API.ProviderPlatform.DynamicOfferDriver.Issue as Issue
import qualified API.ProviderPlatform.DynamicOfferDriver.Maps as Maps
import qualified API.ProviderPlatform.DynamicOfferDriver.Overlay as Overlay
import qualified API.ProviderPlatform.DynamicOfferDriver.Ride as Ride
import qualified API.ProviderPlatform.DynamicOfferDriver.Subscription as Subscription
import qualified API.ProviderPlatform.DynamicOfferDriver.Volunteer as Volunteer
import qualified "lib-dashboard" Domain.Types.Merchant as DM
import "lib-dashboard" Environment
import qualified Kernel.Types.Beckn.City as City
import Kernel.Types.Id
import Servant

-- TODO: Deprecated, Remove after successful deployment
type API =
  "driver-offer"
    :> Capture "merchantId" (ShortId DM.Merchant)
    :> API'

type APIV2 =
  "driver-offer"
    :> Capture "merchantId" (ShortId DM.Merchant)
    :> Capture "city" City.City
    :> API'

type CacAPI =
  "driver-offer"
    :> CacAuth.API

type API' =
  Driver.API
    :<|> Ride.API
    :<|> Subscription.API
    :<|> DriverRegistration.API
    :<|> Issue.API
    :<|> Volunteer.API
    :<|> Overlay.API
    :<|> Maps.API
    :<|> FleetAPI
    :<|> ManagementAPI
    :<|> RideBookingAPI

-- TODO: Deprecated, Remove after successful deployment
handler :: FlowServer API
handler merchantId = do
  let city = getCity merchantId.getShortId
  Driver.handler merchantId city
    :<|> Ride.handler merchantId city
    :<|> Subscription.handler merchantId city
    :<|> DriverRegistration.handler merchantId city
    :<|> Issue.handler merchantId city
    :<|> Volunteer.handler merchantId city
    :<|> Overlay.handler merchantId city
    :<|> Maps.handler merchantId city
    :<|> fleetHandler merchantId city
    :<|> managementHandler merchantId city
    :<|> rideBookingHandler merchantId city
  where
    getCity = \case
      "NAMMA_YATRI_PARTNER" -> City.Bangalore
      "YATRI_PARTNER" -> City.Kochi
      "JATRI_SAATHI_PARTNER" -> City.Kolkata
      _ -> City.AnyCity

handlerV2 :: FlowServer APIV2
handlerV2 merchantId city =
  Driver.handler merchantId city
    :<|> Ride.handler merchantId city
    :<|> Subscription.handler merchantId city
    :<|> DriverRegistration.handler merchantId city
    :<|> Issue.handler merchantId city
    :<|> Volunteer.handler merchantId city
    :<|> Overlay.handler merchantId city
    :<|> Maps.handler merchantId city
    :<|> fleetHandler merchantId city
    :<|> managementHandler merchantId city
    :<|> rideBookingHandler merchantId city

handlerV3 :: FlowServer CacAPI
handlerV3 = CacAuth.handler

type FleetAPI =
  FleetDriverDSL.API

fleetHandler :: ShortId DM.Merchant -> City.City -> FlowServer FleetAPI
fleetHandler =
  FleetDriverDSL.handler

type ManagementAPI =
  ManagementBookingDSL.API
    :<|> ManagementDriverDSL.API
    :<|> ManagementDriverCoinsDSL.API
    :<|> ManagementDriverGoHomeDSL.API
    :<|> ManagementDriverReferralDSL.API
    :<|> ManagementDriverRegistrationDSL.API
    :<|> ManagementMerchantDSL.API
    :<|> ManagementMessageDSL.API
    :<|> ManagementRevenueDSL.API
    :<|> ManagementRideDSL.API
    :<|> NammaTagDSL.API

managementHandler :: ShortId DM.Merchant -> City.City -> FlowServer ManagementAPI
managementHandler merchantId city =
  ManagementBookingDSL.handler merchantId city
    :<|> ManagementDriverDSL.handler merchantId city
    :<|> ManagementDriverCoinsDSL.handler merchantId city
    :<|> ManagementDriverGoHomeDSL.handler merchantId city
    :<|> ManagementDriverReferralDSL.handler merchantId city
    :<|> ManagementDriverRegistrationDSL.handler merchantId city
    :<|> ManagementMerchantDSL.handler merchantId city
    :<|> ManagementMessageDSL.handler merchantId city
    :<|> ManagementRevenueDSL.handler merchantId city
    :<|> ManagementRideDSL.handler merchantId city
    :<|> NammaTagDSL.handler merchantId city

type RideBookingAPI =
  RideBookingDriverDSL.API

rideBookingHandler :: ShortId DM.Merchant -> City.City -> FlowServer RideBookingAPI
rideBookingHandler = RideBookingDriverDSL.handler
