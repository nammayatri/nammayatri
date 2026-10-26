-- | Pass-through like Domain.Action.UI.SharedCab; rider-app checks, under the plate lock, that the driver owns the cab's live
-- session and (for booking actions) that the booking is on that plate.
module Domain.Action.UI.SharedCabBooking
  ( BookingAction (..),
    actionReason,
    bookingAction,
    cabFull,
  )
where

import API.Types.UI.SharedCab (SharedCabSession)
import Domain.Action.UI.SharedCab (DriverAuthInfo, bapInternal, validateSharedCabDriver)
import Environment
import EulerHS.Prelude hiding (id)
import qualified SharedLogic.SharedCab.RiderClient as CallBooking

data BookingAction = Cancel Text | BoardedWithoutCode | Dropped | Accept | Decline
  deriving (Show, Eq)

driverReq :: Maybe Text -> DriverAuthInfo -> Flow CallBooking.BAPDriverReq
driverReq reason (personId, _merchantId, _merchantOpCityId) = do
  vehicle <- validateSharedCabDriver personId
  pure CallBooking.BAPDriverReq {driverId = personId.getId, vehicleNumber = vehicle.registrationNo, reason}

bookingAction :: BookingAction -> DriverAuthInfo -> Text -> Flow SharedCabSession
bookingAction action auth bookingId = do
  req <- driverReq (actionReason action) auth
  bap <- bapInternal
  CallBooking.postBookingAction bap.apiKey bap.url (actionPath action) bookingId req

cabFull :: DriverAuthInfo -> Flow SharedCabSession
cabFull auth = do
  req <- driverReq Nothing auth
  bap <- bapInternal
  CallBooking.postCabFull bap.apiKey bap.url req

actionReason :: BookingAction -> Maybe Text
actionReason = \case
  Cancel reason -> Just reason
  _ -> Nothing

actionPath :: BookingAction -> Text
actionPath = \case
  Cancel _ -> "cancel"
  BoardedWithoutCode -> "boardedWithoutCode"
  Dropped -> "dropped"
  Accept -> "accept"
  Decline -> "decline"
