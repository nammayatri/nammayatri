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
import qualified Domain.Types.Common as SRD
import Environment
import EulerHS.Prelude hiding (id)
import qualified SharedLogic.DriverPool as DP
import qualified SharedLogic.SharedCab.RiderClient as CallBooking

data BookingAction = Cancel Text | BoardedWithoutCode | Dropped | Accept | Decline
  deriving (Show, Eq)

driverReq :: Maybe Text -> DriverAuthInfo -> Flow CallBooking.BAPDriverReq
driverReq reason (personId, _merchantId, _merchantOpCityId) = do
  vehicle <- validateSharedCabDriver personId
  pure CallBooking.BAPDriverReq {driverId = personId.getId, vehicleNumber = vehicle.registrationNo, reason}

bookingAction :: BookingAction -> DriverAuthInfo -> Text -> Flow SharedCabSession
bookingAction action auth@(personId, _merchantId, merchantOpCityId) bookingId = do
  req <- driverReq (actionReason action) auth
  bap <- bapInternal
  session <- CallBooking.postBookingAction bap.apiKey bap.url (actionPath action) bookingId req
  -- the offer's answer goes into the driver's quote-response counters, like a ride request's
  whenJust (offerResponse action) $ DP.recordQuoteResponseCounters merchantOpCityId personId
  pure session

offerResponse :: BookingAction -> Maybe SRD.SearchRequestForDriverResponse
offerResponse = \case
  Accept -> Just SRD.Accept
  Decline -> Just SRD.Reject
  _ -> Nothing

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
