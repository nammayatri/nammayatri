module Screens.TicketBookingFlow.MetroMyTickets.Controller where


import Log 
import Prelude 
import PrestoDOM (Eval, continue)
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


instance showAction :: Show Action where
  show _ = ""

instance loggableAction :: Loggable Action where
  performLog action appId  = case action of
    _ -> pure unit
    
data Action = NoAction

data ScreenOutput = NoOutput


eval :: Action -> MetroMyTicketsScreenState -> Eval Action ScreenOutput MetroMyTicketsScreenState

eval _ state = continue state