{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.RiderPlatform.RideBooking where

import qualified API.Action.RiderPlatform.RideBooking.Booking
import qualified API.Action.RiderPlatform.RideBooking.Cancel
import qualified API.Action.RiderPlatform.RideBooking.Confirm
import qualified API.Action.RiderPlatform.RideBooking.Frontend
import qualified API.Action.RiderPlatform.RideBooking.Maps
import qualified API.Action.RiderPlatform.RideBooking.MultiModal
import qualified API.Action.RiderPlatform.RideBooking.NotifyRideInfo
import qualified API.Action.RiderPlatform.RideBooking.Profile
import qualified API.Action.RiderPlatform.RideBooking.Quote
import qualified API.Action.RiderPlatform.RideBooking.Registration
import qualified API.Action.RiderPlatform.RideBooking.Search
import qualified API.Action.RiderPlatform.RideBooking.Select
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified "lib-dashboard" Environment
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Servant

type API = (API.Action.RiderPlatform.RideBooking.Booking.API :<|> API.Action.RiderPlatform.RideBooking.Cancel.API :<|> API.Action.RiderPlatform.RideBooking.Confirm.API :<|> API.Action.RiderPlatform.RideBooking.Frontend.API :<|> API.Action.RiderPlatform.RideBooking.Maps.API :<|> API.Action.RiderPlatform.RideBooking.MultiModal.API :<|> API.Action.RiderPlatform.RideBooking.NotifyRideInfo.API :<|> API.Action.RiderPlatform.RideBooking.Profile.API :<|> API.Action.RiderPlatform.RideBooking.Quote.API :<|> API.Action.RiderPlatform.RideBooking.Registration.API :<|> API.Action.RiderPlatform.RideBooking.Search.API :<|> API.Action.RiderPlatform.RideBooking.Select.API)

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = API.Action.RiderPlatform.RideBooking.Booking.handler merchantId city :<|> API.Action.RiderPlatform.RideBooking.Cancel.handler merchantId city :<|> API.Action.RiderPlatform.RideBooking.Confirm.handler merchantId city :<|> API.Action.RiderPlatform.RideBooking.Frontend.handler merchantId city :<|> API.Action.RiderPlatform.RideBooking.Maps.handler merchantId city :<|> API.Action.RiderPlatform.RideBooking.MultiModal.handler merchantId city :<|> API.Action.RiderPlatform.RideBooking.NotifyRideInfo.handler merchantId city :<|> API.Action.RiderPlatform.RideBooking.Profile.handler merchantId city :<|> API.Action.RiderPlatform.RideBooking.Quote.handler merchantId city :<|> API.Action.RiderPlatform.RideBooking.Registration.handler merchantId city :<|> API.Action.RiderPlatform.RideBooking.Search.handler merchantId city :<|> API.Action.RiderPlatform.RideBooking.Select.handler merchantId city
