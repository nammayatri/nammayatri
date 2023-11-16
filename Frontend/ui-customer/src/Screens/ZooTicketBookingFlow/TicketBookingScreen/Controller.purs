module Screens.TicketBookingScreen.Controller where

import Log (trackAppActionClick, trackAppEndScreen, trackAppScreenRender, trackAppBackPress, trackAppScreenEvent)
import Prelude (class Show, discard, pure, unit, ($))
import PrestoDOM (Eval, continue, exit, updateAndExit)
import Screens (ScreenName(..), getScreen)
import PrestoDOM.Types.Core (class Loggable)
import Screens.Types (TicketBookingScreenState)

import Components.GenericHeader as GenericHeader

instance showAction :: Show Action where
  show _ = ""

instance loggableAction :: Loggable Action where
  performLog action appId  = case action of
    AfterRender -> trackAppScreenRender appId "screen" (getScreen TICKET_BOOKING_SCREEN)
    _ -> pure unit
    
data Action = AfterRender
            | GenericHeaderAC GenericHeader.Action 

data ScreenOutput = NextScreen String

eval :: Action -> TicketBookingScreenState -> Eval Action ScreenOutput TicketBookingScreenState
eval _ state = continue state