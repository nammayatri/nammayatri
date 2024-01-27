module Screens.TicketBookingFlow.MetroMyTickets.Controller where


import Log 
import Prelude 
import PrestoDOM (Eval, continue, exit)
import Screens 
import Screens.Types 
import Helpers.Utils 
import Effect.Uncurried 
import Effect.Unsafe 
import Screens.Types 
import Common.Types.App as Common
import Language.Strings
import Language.Types
import PrestoDOM.Types.Core (class Loggable)
import Services.API


instance showAction :: Show Action where
  show _ = ""

instance loggableAction :: Loggable Action where
  performLog action appId  = case action of
    _ -> pure unit
    
data Action = NoAction
            | BackPressed
            | AfterRender
            | ActiveTicketPressed MetroTicketBookingStatus
            | PastTicketPressed MetroTicketBookingStatus

data ScreenOutput = NoOutput
                  | GoToMetroTicketDetailsFlow MetroTicketBookingStatus
                  | GoToMetroTicketStatusFlow MetroTicketBookingStatus


eval :: Action -> MetroMyTicketsScreenState -> Eval Action ScreenOutput MetroMyTicketsScreenState


eval (ActiveTicketPressed ticketApiResp) state = exit $ GoToMetroTicketDetailsFlow ticketApiResp

eval (PastTicketPressed ticketApiResp) state = exit $ GoToMetroTicketStatusFlow ticketApiResp

eval AfterRender state =
  continue state {
    props{
      showShimmer = false
    }
  }

eval _ state = continue state