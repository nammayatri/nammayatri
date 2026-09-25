-- | Pass-through like Domain.Action.UI.SharedCab: the driver and vehicle come from the token (the SHARED_CAB variant
-- gate is validateSharedCabDriver); rider-app checks, under the plate lock, that the driver owns the cab's live
-- session and that the booking is on that plate.
module Domain.Action.UI.SharedCabBooking
  ( BookingAction (..),
    bookingAction,
  )
where

import API.Types.UI.SharedCab (SharedCabSession)
import Domain.Action.UI.SharedCab (DriverAuthInfo, bapInternal, validateSharedCabDriver)
import Environment
import EulerHS.Prelude hiding (id)
import Kernel.Types.Id
import qualified SharedLogic.CallSharedCabBooking as CallBooking

data BookingAction = Cancel | BoardedWithoutCode | Dropped
  deriving (Show, Eq)

driverReq :: DriverAuthInfo -> Flow CallBooking.BAPDriverReq
driverReq (personId, _merchantId, _merchantOpCityId) = do
  vehicle <- validateSharedCabDriver personId
  pure CallBooking.BAPDriverReq {driverId = personId.getId, vehicleNumber = vehicle.registrationNo}

bookingAction :: BookingAction -> DriverAuthInfo -> Text -> Flow SharedCabSession
bookingAction action auth bookingId = do
  req <- driverReq auth
  bap <- bapInternal
  CallBooking.postBookingAction bap.apiKey bap.url (actionPath action) bookingId req

actionPath :: BookingAction -> Text
actionPath = \case
  Cancel -> "cancel"
  BoardedWithoutCode -> "boardedWithoutCode"
  Dropped -> "dropped"
