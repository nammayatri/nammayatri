{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.Dashboard.RideBooking
  ( API,
    handler,
  )
where

import qualified API.Action.Dashboard.RideBooking.Booking
import qualified API.Action.Dashboard.RideBooking.Cancel
import qualified API.Action.Dashboard.RideBooking.Confirm
import qualified API.Action.Dashboard.RideBooking.Frontend
import qualified API.Action.Dashboard.RideBooking.Maps
import qualified API.Action.Dashboard.RideBooking.MultiModal
import qualified API.Action.Dashboard.RideBooking.NotifyRideInfo
import qualified API.Action.Dashboard.RideBooking.Profile
import qualified API.Action.Dashboard.RideBooking.Quote
import qualified API.Action.Dashboard.RideBooking.Registration
import qualified API.Action.Dashboard.RideBooking.Search
import qualified API.Action.Dashboard.RideBooking.Select
import qualified Domain.Types.Merchant
import qualified Environment
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Servant

type API = (API.Action.Dashboard.RideBooking.Booking.API :<|> API.Action.Dashboard.RideBooking.Cancel.API :<|> API.Action.Dashboard.RideBooking.Confirm.API :<|> API.Action.Dashboard.RideBooking.Frontend.API :<|> API.Action.Dashboard.RideBooking.Maps.API :<|> API.Action.Dashboard.RideBooking.MultiModal.API :<|> API.Action.Dashboard.RideBooking.NotifyRideInfo.API :<|> API.Action.Dashboard.RideBooking.Profile.API :<|> API.Action.Dashboard.RideBooking.Quote.API :<|> API.Action.Dashboard.RideBooking.Registration.API :<|> API.Action.Dashboard.RideBooking.Search.API :<|> API.Action.Dashboard.RideBooking.Select.API)

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = API.Action.Dashboard.RideBooking.Booking.handler merchantId city :<|> API.Action.Dashboard.RideBooking.Cancel.handler merchantId city :<|> API.Action.Dashboard.RideBooking.Confirm.handler merchantId city :<|> API.Action.Dashboard.RideBooking.Frontend.handler merchantId city :<|> API.Action.Dashboard.RideBooking.Maps.handler merchantId city :<|> API.Action.Dashboard.RideBooking.MultiModal.handler merchantId city :<|> API.Action.Dashboard.RideBooking.NotifyRideInfo.handler merchantId city :<|> API.Action.Dashboard.RideBooking.Profile.handler merchantId city :<|> API.Action.Dashboard.RideBooking.Quote.handler merchantId city :<|> API.Action.Dashboard.RideBooking.Registration.handler merchantId city :<|> API.Action.Dashboard.RideBooking.Search.handler merchantId city :<|> API.Action.Dashboard.RideBooking.Select.handler merchantId city
