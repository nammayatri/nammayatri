{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.DashboardAuth.RideBooking
  ( API,
    handler,
  )
where

import qualified API.Action.DashboardAuth.RideBooking.AddBaggage
import qualified API.Action.DashboardAuth.RideBooking.Booking
import qualified API.Action.DashboardAuth.RideBooking.Cancel
import qualified API.Action.DashboardAuth.RideBooking.ChangeServiceTier
import qualified API.Action.DashboardAuth.RideBooking.Confirm
import qualified API.Action.DashboardAuth.RideBooking.Frontend
import qualified API.Action.DashboardAuth.RideBooking.Maps
import qualified API.Action.DashboardAuth.RideBooking.MultiModal
import qualified API.Action.DashboardAuth.RideBooking.NotifyRideInfo
import qualified API.Action.DashboardAuth.RideBooking.Profile
import qualified API.Action.DashboardAuth.RideBooking.Quote
import qualified API.Action.DashboardAuth.RideBooking.Registration
import qualified API.Action.DashboardAuth.RideBooking.Search
import qualified API.Action.DashboardAuth.RideBooking.Select
import qualified Domain.Types.Merchant
import qualified Environment
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Servant

type API = (API.Action.DashboardAuth.RideBooking.AddBaggage.API :<|> API.Action.DashboardAuth.RideBooking.Booking.API :<|> API.Action.DashboardAuth.RideBooking.Cancel.API :<|> API.Action.DashboardAuth.RideBooking.ChangeServiceTier.API :<|> API.Action.DashboardAuth.RideBooking.Confirm.API :<|> API.Action.DashboardAuth.RideBooking.Frontend.API :<|> API.Action.DashboardAuth.RideBooking.Maps.API :<|> API.Action.DashboardAuth.RideBooking.MultiModal.API :<|> API.Action.DashboardAuth.RideBooking.NotifyRideInfo.API :<|> API.Action.DashboardAuth.RideBooking.Profile.API :<|> API.Action.DashboardAuth.RideBooking.Quote.API :<|> API.Action.DashboardAuth.RideBooking.Registration.API :<|> API.Action.DashboardAuth.RideBooking.Search.API :<|> API.Action.DashboardAuth.RideBooking.Select.API)

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = API.Action.DashboardAuth.RideBooking.AddBaggage.handler merchantId city :<|> API.Action.DashboardAuth.RideBooking.Booking.handler merchantId city :<|> API.Action.DashboardAuth.RideBooking.Cancel.handler merchantId city :<|> API.Action.DashboardAuth.RideBooking.ChangeServiceTier.handler merchantId city :<|> API.Action.DashboardAuth.RideBooking.Confirm.handler merchantId city :<|> API.Action.DashboardAuth.RideBooking.Frontend.handler merchantId city :<|> API.Action.DashboardAuth.RideBooking.Maps.handler merchantId city :<|> API.Action.DashboardAuth.RideBooking.MultiModal.handler merchantId city :<|> API.Action.DashboardAuth.RideBooking.NotifyRideInfo.handler merchantId city :<|> API.Action.DashboardAuth.RideBooking.Profile.handler merchantId city :<|> API.Action.DashboardAuth.RideBooking.Quote.handler merchantId city :<|> API.Action.DashboardAuth.RideBooking.Registration.handler merchantId city :<|> API.Action.DashboardAuth.RideBooking.Search.handler merchantId city :<|> API.Action.DashboardAuth.RideBooking.Select.handler merchantId city
