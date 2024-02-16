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
    handler,
    handlerV2,
  )
where

import qualified API.ProviderPlatform.DynamicOfferDriver.Booking as Booking
import qualified API.ProviderPlatform.DynamicOfferDriver.Driver as Driver
import qualified API.ProviderPlatform.DynamicOfferDriver.Driver.Coin as DriverCoin
import qualified API.ProviderPlatform.DynamicOfferDriver.Driver.Registration as DriverRegistration
import qualified API.ProviderPlatform.DynamicOfferDriver.DriverReferral as DriverReferral
import qualified API.ProviderPlatform.DynamicOfferDriver.Issue as Issue
import qualified API.ProviderPlatform.DynamicOfferDriver.Maps as Maps
import qualified API.ProviderPlatform.DynamicOfferDriver.Merchant as Merchant
import qualified API.ProviderPlatform.DynamicOfferDriver.Message as Message
import qualified API.ProviderPlatform.DynamicOfferDriver.Overlay as Overlay
import qualified API.ProviderPlatform.DynamicOfferDriver.Revenue as Revenue
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

type API' =
  Driver.API
    :<|> Ride.API
    :<|> Subscription.API
    :<|> Booking.API
    :<|> Merchant.API
    :<|> Message.API
    :<|> DriverReferral.API
    :<|> DriverRegistration.API
    :<|> DriverCoin.API
    :<|> Issue.API
    :<|> Volunteer.API
    :<|> Revenue.API
    :<|> Overlay.API
    :<|> Maps.API

-- TODO: Deprecated, Remove after successful deployment
handler :: FlowServer API
handler merchantId = do
  let city = getCity merchantId.getShortId
  Driver.handler merchantId city
    :<|> Ride.handler merchantId city
    :<|> Subscription.handler merchantId city
    :<|> Booking.handler merchantId city
    :<|> Merchant.handler merchantId city
    :<|> Message.handler merchantId city
    :<|> DriverReferral.handler merchantId city
    :<|> DriverRegistration.handler merchantId city
    :<|> DriverCoin.handler merchantId city
    :<|> Issue.handler merchantId city
    :<|> Volunteer.handler merchantId city
    :<|> Revenue.handler merchantId city
    :<|> Overlay.handler merchantId city
    :<|> Maps.handler merchantId city
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
    :<|> Booking.handler merchantId city
    :<|> Merchant.handler merchantId city
    :<|> Message.handler merchantId city
    :<|> DriverReferral.handler merchantId city
    :<|> DriverRegistration.handler merchantId city
    :<|> DriverCoin.handler merchantId city
    :<|> Issue.handler merchantId city
    :<|> Volunteer.handler merchantId city
    :<|> Revenue.handler merchantId city
    :<|> Overlay.handler merchantId city
    :<|> Maps.handler merchantId city
