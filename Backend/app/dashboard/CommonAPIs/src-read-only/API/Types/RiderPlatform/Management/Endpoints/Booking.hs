{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE StandaloneKindSignatures #-}
module API.Types.RiderPlatform.Management.Endpoints.Booking where
import EulerHS.Prelude hiding (id, state)
import Servant
import Data.OpenApi (ToSchema)
import Servant.Client
import Kernel.Types.Common
import qualified Dashboard.Common.Booking
import qualified EulerHS.Types
import qualified Data.Singletons.TH



type API = ("booking" :> (PostBookingCancelAllStuck :<|> PostBookingSyncMultiple))
type PostBookingCancelAllStuck = ("cancel" :> "allStuck" :> ReqBody ('[JSON]) Dashboard.Common.Booking.StuckBookingsCancelReq :> Post ('[JSON]) Dashboard.Common.Booking.StuckBookingsCancelRes)
type PostBookingSyncMultiple = ("sync" :> ReqBody ('[JSON]) Dashboard.Common.Booking.MultipleBookingSyncReq :> Post ('[JSON]) Dashboard.Common.Booking.MultipleBookingSyncResp)
data BookingAPIs
    = BookingAPIs {postBookingCancelAllStuck :: (Dashboard.Common.Booking.StuckBookingsCancelReq -> EulerHS.Types.EulerClient Dashboard.Common.Booking.StuckBookingsCancelRes),
                   postBookingSyncMultiple :: (Dashboard.Common.Booking.MultipleBookingSyncReq -> EulerHS.Types.EulerClient Dashboard.Common.Booking.MultipleBookingSyncResp)}
mkBookingAPIs :: (Client EulerHS.Types.EulerClient API -> BookingAPIs)
mkBookingAPIs bookingClient = (BookingAPIs {..})
                  where postBookingCancelAllStuck :<|> postBookingSyncMultiple = bookingClient
data BookingUserActionType
    = POST_BOOKING_CANCEL_ALL_STUCK | POST_BOOKING_SYNC_MULTIPLE
    deriving stock (Show, Read, Generic, Eq, Ord)
    deriving anyclass (ToJSON, FromJSON, ToSchema)

$(Data.Singletons.TH.genSingletons [(''BookingUserActionType)])

