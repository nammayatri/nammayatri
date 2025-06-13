{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.ProviderPlatform.RideBooking where

import qualified API.Action.ProviderPlatform.RideBooking.Driver
import qualified API.Action.ProviderPlatform.RideBooking.DriverRegistration
import qualified API.Action.ProviderPlatform.RideBooking.Maps
import qualified API.Action.ProviderPlatform.RideBooking.MeterRide
import qualified API.Action.ProviderPlatform.RideBooking.Ride
import qualified API.Action.ProviderPlatform.RideBooking.SearchRequest
import qualified API.Action.ProviderPlatform.RideBooking.Volunteer
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified "lib-dashboard" Environment
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Servant

type API = (API.Action.ProviderPlatform.RideBooking.Driver.API :<|> API.Action.ProviderPlatform.RideBooking.DriverRegistration.API :<|> API.Action.ProviderPlatform.RideBooking.Maps.API :<|> API.Action.ProviderPlatform.RideBooking.MeterRide.API :<|> API.Action.ProviderPlatform.RideBooking.Ride.API :<|> API.Action.ProviderPlatform.RideBooking.SearchRequest.API :<|> API.Action.ProviderPlatform.RideBooking.Volunteer.API)

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = API.Action.ProviderPlatform.RideBooking.Driver.handler merchantId city :<|> API.Action.ProviderPlatform.RideBooking.DriverRegistration.handler merchantId city :<|> API.Action.ProviderPlatform.RideBooking.Maps.handler merchantId city :<|> API.Action.ProviderPlatform.RideBooking.MeterRide.handler merchantId city :<|> API.Action.ProviderPlatform.RideBooking.Ride.handler merchantId city :<|> API.Action.ProviderPlatform.RideBooking.SearchRequest.handler merchantId city :<|> API.Action.ProviderPlatform.RideBooking.Volunteer.handler merchantId city
