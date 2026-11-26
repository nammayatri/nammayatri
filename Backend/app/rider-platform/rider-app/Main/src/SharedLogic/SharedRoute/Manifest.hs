{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- | The passengers of one shared-route run and the driver's verbs on them. A run is the driver platform's ride; its
-- passengers are the CONFIRMED ticket bookings placed on it (frfs_ticket_booking.ride_id). Nothing here is a new state:
-- a board flips the booking's tickets to INPROGRESS, a drop to USED, a no-show counts on the booking, exactly the
-- transitions the shared cab uses, and 'passengerStateOf' reads them back. A drop that leaves nothing to board ends the journey. The driver platform reads and acts on them
-- through the shared cab's session and booking-operation endpoints.
module SharedLogic.SharedRoute.Manifest
  ( RunPassenger (..),
    runPassengers,
    RunAction (..),
    applyRunAction,
  )
where

import Data.Maybe (listToMaybe)
import qualified Data.Text as T
import qualified Domain.Types.FRFSTicket as DFRFSTicket
import qualified Domain.Types.FRFSTicketBooking as DFTB
import qualified Domain.Types.FRFSTicketBookingStatus as DFRFSTicketBookingStatus
import qualified Domain.Types.FRFSTicketStatus as DFRFSTicketStatus
import qualified Domain.Types.Journey as DJourney
import Domain.Types.SharedRoute (SharedDemandKnobs (..), SharedPassengerState)
import qualified Environment
import EulerHS.Prelude hiding (id)
import Kernel.Types.Error
import Kernel.Utils.Common
import SharedLogic.FRFSCancelJourney (endJourneyIfOnlyTransitLeg)
import qualified SharedLogic.IntegratedBPPConfig as SIBC
import qualified SharedLogic.SharedRoute.Knobs as Knobs
import SharedLogic.SharedRoute.Passenger (passengerStateOf)
import qualified Storage.Queries.FRFSTicket as QFRFSTicket
import qualified Storage.Queries.FRFSTicketBooking as QFRFSTicketBooking
import qualified Storage.Queries.PersonExtra as QPersonExtra
import qualified Tools.Fields as Fields

data RunPassenger = RunPassenger
  { booking :: DFTB.FRFSTicketBooking,
    tickets :: [DFRFSTicket.FRFSTicket],
    firstName :: Text,
    state :: SharedPassengerState
  }

-- | Every passenger of the run, by booking, with where they stand; cancelled bookings are not on it.
runPassengers :: Text -> Environment.Flow [RunPassenger]
runPassengers rideId = do
  bookings <- QFRFSTicketBooking.findAllConfirmedByRideId rideId
  tickets <- if null bookings then pure [] else QFRFSTicket.findAllByTicketBookingIds (map (.id) bookings)
  persons <- if null bookings then pure [] else QPersonExtra.findAllByIds (map (.riderId) bookings)
  pure
    [ RunPassenger
        { booking,
          tickets = ticketsOf,
          firstName = fromMaybe "Rider" $ listToMaybe [w | p <- persons, p.id == booking.riderId, Just n <- [p.firstName], w <- T.words n],
          state = passengerStateOf booking ticketsOf
        }
      | booking <- bookings,
        let ticketsOf = filter ((== booking.id) . (.frfsTicketBookingId)) tickets
    ]

data RunAction
  = -- | the driver boarded them, with the rider's per-booking OTP when the feed asks for one
    Board (Maybe Text)
  | -- | the driver gave up waiting for them at their stop
    NoShow
  | -- | the driver dropped them
    Drop
  deriving (Show, Eq)

-- | The booking must be live on this run. A repeated board or drop is a no-op; a no-show after boarding is refused.
-- @vehicleNumber@ is what the ticket records as the scanning vehicle, as a conductor's scan would.
applyRunAction :: Text -> DFTB.FRFSTicketBooking -> RunAction -> Text -> Environment.Flow ()
applyRunAction rideId booking action vehicleNumber = do
  unless (booking.rideId == Just rideId) $ throwError (InvalidRequest $ "booking " <> booking.id.getId <> " is not on run " <> rideId)
  unless (booking.status == DFRFSTicketBookingStatus.CONFIRMED) $ throwError (InvalidRequest $ "booking " <> booking.id.getId <> " is not live")
  tickets <- QFRFSTicket.findAllByTicketBookingId booking.id
  let statuses = map (.status) tickets
      boarded = DFRFSTicketStatus.INPROGRESS `elem` statuses || DFRFSTicketStatus.USED `elem` statuses
  case action of
    Board mbOtp
      | boarded -> pure ()
      | otherwise -> do
        knobs <- demandKnobsOf booking
        -- The boarding knob decides what the driver's tap means: under NO_CHECK it admits, under OTP the booking's
        -- code must match (module S lands the hash and the compare), under anything else it is refused.
        case Knobs.verifyBoarding knobs.boarding (maybe Knobs.NoEvidence Knobs.PresentedOtp mbOtp) of
          Knobs.Admit -> board tickets
          Knobs.RejectBoarding why -> throwError (InvalidRequest why)
          Knobs.CheckOtp _ -> Knobs.notImplemented "OTP boarding: compare against the booking's OTP hash (module S)"
          other -> Knobs.notImplemented ("boarding a run passenger under a knob that asks for " <> show other)
    NoShow
      | boarded -> throwError (InvalidRequest $ "booking " <> booking.id.getId <> " has already boarded")
      -- the consequence of the no-show (a trip off the subscription, a strike) is the subscription module's (S); the
      -- count is what it and the manifest read
      | otherwise -> QFRFSTicketBooking.updateSharedCabNoShowsById (booking.sharedCabNoShows + 1) booking.id
    Drop
      | not boarded -> throwError (InvalidRequest $ "booking " <> booking.id.getId <> " has not boarded")
      | otherwise -> do
        forM_ (filter ((== DFRFSTicketStatus.INPROGRESS) . (.status)) tickets) $ \ticket ->
          QFRFSTicket.updateStatusByTBookingIdAndTicketNumber DFRFSTicketStatus.USED ticket.scannedByVehicleNumber booking.id ticket.ticketNumber
        -- the ride is over once no ticket is left to board: end the rider's journey. A failure reaches the driver's call,
        -- which can repeat the drop: the tickets are USED by then, so the repeat passes the boarded guard and ends it again.
        unless (DFRFSTicketStatus.ACTIVE `elem` statuses) $
          endJourneyIfOnlyTransitLeg DJourney.FEEDBACK_PENDING booking.searchId.getId
  where
    board tickets = do
      forM_ (filter ((== DFRFSTicketStatus.ACTIVE) . (.status)) tickets) $ \ticket ->
        QFRFSTicket.updateStatusByTBookingIdAndTicketNumber DFRFSTicketStatus.INPROGRESS (Just vehicleNumber) booking.id ticket.ticketNumber
      QFRFSTicketBooking.updateFRFSTicketBookingVehicleNumberById (Just vehicleNumber) booking.id

demandKnobsOf :: DFTB.FRFSTicketBooking -> Environment.Flow SharedDemandKnobs
demandKnobsOf booking = SIBC.findIntegratedBPPConfigById booking.integratedBppConfigId >>= Knobs.requireDemandKnobs
