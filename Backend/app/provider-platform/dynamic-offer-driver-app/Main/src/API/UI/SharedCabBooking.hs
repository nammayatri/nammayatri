-- | The driver's actions on one booking, forwarded to rider-app /internal/sharedCab/booking/{bookingId}/{action}
-- (04 §4 D5, D7), and R19's cab-full. Paths and response are the driver UI contract
-- (sharedcab-driver-api-types.ts: cancelBooking, boardedWithoutCode, dropped -> SharedCabSession).
module API.UI.SharedCabBooking
  ( API,
    handler,
  )
where

import API.Types.UI.SharedCab (SharedCabSession)
import qualified Domain.Action.UI.SharedCab as DSharedCab
import qualified Domain.Action.UI.SharedCabBooking as DSharedCabBooking
import Environment
import EulerHS.Prelude
import Kernel.Utils.Common
import Servant
import Tools.Auth

type BookingAction name =
  "sharedCab"
    :> "booking"
    :> Capture "bookingId" Text
    :> name
    :> TokenAuth
    :> Post '[JSON] SharedCabSession

type API =
  BookingAction "cancel"
    :<|> BookingAction "boardedWithoutCode"
    :<|> BookingAction "dropped"
    :<|> "sharedCab" :> "cabFull" :> TokenAuth :> Post '[JSON] SharedCabSession

handler :: FlowServer API
handler =
  act DSharedCabBooking.Cancel
    :<|> act DSharedCabBooking.BoardedWithoutCode
    :<|> act DSharedCabBooking.Dropped
    :<|> cabFull

act :: DSharedCabBooking.BookingAction -> Text -> DSharedCab.DriverAuthInfo -> FlowHandler SharedCabSession
act action bookingId auth@(personId, _, _) =
  withFlowHandlerAPI $ withPersonIdLogTag personId $ DSharedCabBooking.bookingAction action auth bookingId

cabFull :: DSharedCab.DriverAuthInfo -> FlowHandler SharedCabSession
cabFull auth@(personId, _, _) = withFlowHandlerAPI $ withPersonIdLogTag personId $ DSharedCabBooking.cabFull auth
