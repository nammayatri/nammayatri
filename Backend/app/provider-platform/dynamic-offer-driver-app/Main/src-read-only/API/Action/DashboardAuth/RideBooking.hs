{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.DashboardAuth.RideBooking
  ( API,
    handler,
  )
where

import qualified API.Action.DashboardAuth.RideBooking.Driver
import qualified API.Action.DashboardAuth.RideBooking.DriverRegistration
import qualified API.Action.DashboardAuth.RideBooking.Maps
import qualified API.Action.DashboardAuth.RideBooking.MeterRide
import qualified API.Action.DashboardAuth.RideBooking.Ride
import qualified API.Action.DashboardAuth.RideBooking.SearchRequest
import qualified API.Action.DashboardAuth.RideBooking.Volunteer
import qualified Domain.Types.Merchant
import qualified Environment
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Servant

type API = (API.Action.DashboardAuth.RideBooking.Driver.API :<|> API.Action.DashboardAuth.RideBooking.DriverRegistration.API :<|> API.Action.DashboardAuth.RideBooking.Maps.API :<|> API.Action.DashboardAuth.RideBooking.MeterRide.API :<|> API.Action.DashboardAuth.RideBooking.Ride.API :<|> API.Action.DashboardAuth.RideBooking.SearchRequest.API :<|> API.Action.DashboardAuth.RideBooking.Volunteer.API)

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = API.Action.DashboardAuth.RideBooking.Driver.handler merchantId city :<|> API.Action.DashboardAuth.RideBooking.DriverRegistration.handler merchantId city :<|> API.Action.DashboardAuth.RideBooking.Maps.handler merchantId city :<|> API.Action.DashboardAuth.RideBooking.MeterRide.handler merchantId city :<|> API.Action.DashboardAuth.RideBooking.Ride.handler merchantId city :<|> API.Action.DashboardAuth.RideBooking.SearchRequest.handler merchantId city :<|> API.Action.DashboardAuth.RideBooking.Volunteer.handler merchantId city
