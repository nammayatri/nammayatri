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
    handler,
  )
where

import qualified API.ProviderPlatform.DynamicOfferDriver.Booking as Booking
import qualified API.ProviderPlatform.DynamicOfferDriver.Driver as Driver
import qualified API.ProviderPlatform.DynamicOfferDriver.Driver.Registration as DriverRegistration
import qualified API.ProviderPlatform.DynamicOfferDriver.DriverReferral as DriverReferral
import qualified API.ProviderPlatform.DynamicOfferDriver.Issue as Issue
import qualified API.ProviderPlatform.DynamicOfferDriver.Merchant as Merchant
import qualified API.ProviderPlatform.DynamicOfferDriver.Message as Message
import qualified API.ProviderPlatform.DynamicOfferDriver.Overlay as Overlay
import qualified API.ProviderPlatform.DynamicOfferDriver.RegistryMap as RegistryMap
import qualified API.ProviderPlatform.DynamicOfferDriver.Revenue as Revenue
import qualified API.ProviderPlatform.DynamicOfferDriver.Ride as Ride
import qualified API.ProviderPlatform.DynamicOfferDriver.Subscription as Subscription
import qualified API.ProviderPlatform.DynamicOfferDriver.Volunteer as Volunteer
import qualified "lib-dashboard" Domain.Types.Merchant as DM
import "lib-dashboard" Environment
import Kernel.Types.Id
import Servant

type API =
  "driver-offer"
    :> Capture "merchantId" (ShortId DM.Merchant)
    :> ( Driver.API
           :<|> Ride.API
           :<|> Subscription.API
           :<|> Booking.API
           :<|> Merchant.API
           :<|> Message.API
           :<|> DriverReferral.API
           :<|> DriverRegistration.API
           :<|> Issue.API
           :<|> Volunteer.API
           :<|> Revenue.API
           :<|> Overlay.API
       )

handler :: FlowServer API
handler merchantId =
  Driver.handler merchantId
    :<|> Ride.handler merchantId
    :<|> Subscription.handler merchantId
    :<|> Booking.handler merchantId
    :<|> Merchant.handler merchantId
    :<|> Message.handler merchantId
    :<|> DriverReferral.handler merchantId
    :<|> DriverRegistration.handler merchantId
    :<|> Issue.handler merchantId
    :<|> Volunteer.handler merchantId
    :<|> Revenue.handler merchantId
    :<|> Overlay.handler merchantId
