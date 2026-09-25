-- | The driver's actions on one booking, forwarded to rider-app /internal/sharedCab/booking/{bookingId}/{action}
-- (04 §4 D5, D7). Paths and response are the driver UI contract (sharedcab-driver-api-types.ts: cancelBooking,
-- boardedWithoutCode, dropped -> SharedCabSession).
module API.UI.SharedCabBooking
  ( API,
    handler,
  )
where

import API.Types.UI.SharedCab (SharedCabSession)
import qualified Domain.Action.UI.SharedCabBooking as DSharedCabBooking
import qualified Domain.Types.Merchant as Merchant
import qualified Domain.Types.MerchantOperatingCity as MerchantOperatingCity
import qualified Domain.Types.Person as Person
import Environment
import EulerHS.Prelude
import Kernel.Types.Id
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

type DriverAuth = (Id Person.Person, Id Merchant.Merchant, Id MerchantOperatingCity.MerchantOperatingCity)

handler :: FlowServer API
handler =
  act DSharedCabBooking.Cancel
    :<|> act DSharedCabBooking.BoardedWithoutCode
    :<|> act DSharedCabBooking.Dropped

act :: DSharedCabBooking.BookingAction -> Text -> DriverAuth -> FlowHandler SharedCabSession
act action bookingId auth@(personId, _, _) =
  withFlowHandlerAPI $ withPersonIdLogTag personId $ DSharedCabBooking.bookingAction action auth bookingId
