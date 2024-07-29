{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.RiderPlatform.Management.Booking where

import qualified Dashboard.Common.Booking
import Data.OpenApi (ToSchema)
import EulerHS.Prelude hiding (id)
import qualified EulerHS.Types
import Servant
import Servant.Client

type API = ("booking" :> (PostBookingCancelAllStuck :<|> PostBookingSyncMultiple))

type PostBookingCancelAllStuck = ("cancel" :> "allStuck" :> ReqBody '[JSON] Dashboard.Common.Booking.StuckBookingsCancelReq :> Post '[JSON] Dashboard.Common.Booking.StuckBookingsCancelRes)

type PostBookingSyncMultiple = ("sync" :> ReqBody '[JSON] Dashboard.Common.Booking.MultipleBookingSyncReq :> Post '[JSON] Dashboard.Common.Booking.MultipleBookingSyncResp)

data BookingAPIs = BookingAPIs
  { postBookingCancelAllStuck :: Dashboard.Common.Booking.StuckBookingsCancelReq -> EulerHS.Types.EulerClient Dashboard.Common.Booking.StuckBookingsCancelRes,
    postBookingSyncMultiple :: Dashboard.Common.Booking.MultipleBookingSyncReq -> EulerHS.Types.EulerClient Dashboard.Common.Booking.MultipleBookingSyncResp
  }

mkBookingAPIs :: (Client EulerHS.Types.EulerClient API -> BookingAPIs)
mkBookingAPIs bookingClient = (BookingAPIs {..})
  where
    postBookingCancelAllStuck :<|> postBookingSyncMultiple = bookingClient
