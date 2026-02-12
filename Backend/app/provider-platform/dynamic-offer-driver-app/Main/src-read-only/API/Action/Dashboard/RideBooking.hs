{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.Dashboard.RideBooking
  ( API,
    handler,
  )
where

import qualified API.Action.Dashboard.RideBooking.Driver
import qualified API.Action.Dashboard.RideBooking.DriverRegistration
import qualified API.Action.Dashboard.RideBooking.Maps
import qualified API.Action.Dashboard.RideBooking.MeterRide
import qualified API.Action.Dashboard.RideBooking.Ride
import qualified API.Action.Dashboard.RideBooking.SearchRequest
import qualified API.Action.Dashboard.RideBooking.Volunteer
import qualified Domain.Types.Merchant
import qualified Environment
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Servant

type API = (API.Action.Dashboard.RideBooking.Driver.API :<|> API.Action.Dashboard.RideBooking.DriverRegistration.API :<|> API.Action.Dashboard.RideBooking.Maps.API :<|> API.Action.Dashboard.RideBooking.MeterRide.API :<|> API.Action.Dashboard.RideBooking.Ride.API :<|> API.Action.Dashboard.RideBooking.SearchRequest.API :<|> API.Action.Dashboard.RideBooking.Volunteer.API)

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = API.Action.Dashboard.RideBooking.Driver.handler merchantId city :<|> API.Action.Dashboard.RideBooking.DriverRegistration.handler merchantId city :<|> API.Action.Dashboard.RideBooking.Maps.handler merchantId city :<|> API.Action.Dashboard.RideBooking.MeterRide.handler merchantId city :<|> API.Action.Dashboard.RideBooking.Ride.handler merchantId city :<|> API.Action.Dashboard.RideBooking.SearchRequest.handler merchantId city :<|> API.Action.Dashboard.RideBooking.Volunteer.handler merchantId city
