-- | A run's passenger as the driver's manifest shows them, derived from the ticket booking and its tickets.
-- The encoding is the one written on 'SharedPassengerState': no new column, the shared cab's existing
-- ticket transitions (INPROGRESS on board, USED on drop) and no-show counter carry it.
module SharedLogic.SharedRoute.Passenger
  ( passengerStateOf,
  )
where

import qualified Domain.Types.FRFSTicket as DFRFSTicket
import qualified Domain.Types.FRFSTicketBooking as DFRFSTicketBooking
import qualified Domain.Types.FRFSTicketBookingStatus as DFRFSTicketBookingStatus
import qualified Domain.Types.FRFSTicketStatus as DFRFSTicketStatus
import Domain.Types.SharedRoute (SharedPassengerState (..))
import Kernel.Prelude

passengerStateOf :: DFRFSTicketBooking.FRFSTicketBooking -> [DFRFSTicket.FRFSTicket] -> SharedPassengerState
passengerStateOf booking tickets
  | booking.status == DFRFSTicketBookingStatus.CANCELLED = CANCELLED
  | anyTicket DFRFSTicketStatus.USED = DROPPED
  | anyTicket DFRFSTicketStatus.INPROGRESS = BOARDED
  | booking.sharedCabNoShows > 0 = NO_SHOW
  | otherwise = WAITING
  where
    anyTicket st = any (\t -> t.status == st) tickets
