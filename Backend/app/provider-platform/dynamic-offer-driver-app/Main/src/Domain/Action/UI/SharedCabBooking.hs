-- | Pass-through like Domain.Action.UI.SharedCab; rider-app checks, under the plate lock, that the driver owns the cab's live
-- session and (for booking actions) that the booking is on that plate.
module Domain.Action.UI.SharedCabBooking
  ( BookingAction (..),
    actionReason,
    bookingActionOf,
    bookingOperation,
  )
where

import API.Types.UI.SharedCab (SharedCabBookingOperation (..), SharedCabBookingOperationReq (..))
import Domain.Action.UI.SharedCabExtra (DriverAuthInfo, bapInternal, validateSharedCabDriver)
import qualified Domain.Types.Common as SRD
import Domain.Types.SharedCabSession (SharedCabSession)
import Environment
import EulerHS.Prelude hiding (id)
import Kernel.Types.Error (GenericError (InvalidRequest))
import Kernel.Utils.Common (fromMaybeM)
import qualified SharedLogic.DriverPool as DP
import qualified SharedLogic.SharedCab.RiderClient as CallBooking

data BookingAction = Cancel Text | BoardedWithoutCode | Dropped | Accept | Decline
  deriving (Show, Eq)

bookingActionOf :: SharedCabBookingOperationReq -> Maybe BookingAction
bookingActionOf req = case req.operation of
  ACCEPT -> Just Accept
  DECLINE -> Just Decline
  DROPPED -> Just Dropped
  BOARDED_WITHOUT_CODE -> Just BoardedWithoutCode
  CANCEL -> Cancel <$> req.reason

bookingOperation :: DriverAuthInfo -> Text -> SharedCabBookingOperationReq -> Flow SharedCabSession
bookingOperation (personId, _merchantId, merchantOpCityId) bookingId req = do
  action <- fromMaybeM (InvalidRequest "reason is required to cancel a booking") (bookingActionOf req)
  vehicle <- validateSharedCabDriver personId
  bap <- bapInternal
  session <-
    CallBooking.postBookingOperation bap.apiKey bap.url bookingId $
      CallBooking.BAPBookingOperationReq {operation = req.operation, driverId = personId.getId, vehicleNumber = vehicle.registrationNo, reason = actionReason action}
  -- the offer's answer goes into the driver's quote-response counters, like a ride request's
  whenJust (offerResponse action) $ DP.recordQuoteResponseCounters merchantOpCityId personId
  pure session

offerResponse :: BookingAction -> Maybe SRD.SearchRequestForDriverResponse
offerResponse = \case
  Accept -> Just SRD.Accept
  Decline -> Just SRD.Reject
  _ -> Nothing

actionReason :: BookingAction -> Maybe Text
actionReason = \case
  Cancel reason -> Just reason
  _ -> Nothing
