module Screens.TicketBookingScreen.Controller where

import Log (trackAppActionClick, trackAppEndScreen, trackAppScreenRender, trackAppBackPress, trackAppScreenEvent)
import Prelude (class Show, discard, pure, unit, ($), not)
import PrestoDOM (Eval, continue, exit, updateAndExit)
import Screens (ScreenName(..), getScreen)
import PrestoDOM.Types.Core (class Loggable)
import Screens.Types (TicketBookingScreenState)

import Components.GenericHeader as GenericHeader
import Components.PrimaryButton as PrimaryButton

instance showAction :: Show Action where
  show _ = ""

instance loggableAction :: Loggable Action where
  performLog action appId  = case action of
    AfterRender -> trackAppScreenRender appId "screen" (getScreen TICKET_BOOKING_SCREEN)
    _ -> pure unit
    
data Action = AfterRender
            | GenericHeaderAC GenericHeader.Action 
            | PrimaryButtonAC PrimaryButton.Action
            | ToggleTicketOption String 

data ScreenOutput = NextScreen String

eval :: Action -> TicketBookingScreenState -> Eval Action ScreenOutput TicketBookingScreenState
eval (ToggleTicketOption ticketID) state = 
  case ticketID of 
    "ZOO_ENTRY" -> continue state{data{zooEntry {availed = not (state.data.zooEntry.availed)}}}
    "AQUARIUM_ENTRY" -> continue state{data{aquariumEntry {availed = not (state.data.aquariumEntry.availed)}}}
    "PHOTO_OR_VIDEOGRAPHY" -> continue state{data{photoOrVideoGraphy {availed = not (state.data.photoOrVideoGraphy.availed)}}}
    _ -> continue state
eval _ state = continue state