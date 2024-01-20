module Screens.TicketBookingFlow.MetroTicketBooking.Controller where

import Prelude
import Screens.Types as ST
import Components.GenericHeader as GenericHeader
-- import Components.SelectionTabModal as SelectionTabModal
import Components.PrimaryEditText as PrimaryEditText
-- import Components.IncrementDecrementModel as IncrementDecrementModel
-- import Components.GenericHeader.Controller (Action(..)) as GenericHeaderController
-- import Components.PrimaryButton.Controller as PrimaryButtonController
import Components.PrimaryButton as PrimaryButton
import Prelude (class Show, pure, unit, bind, discard, ($), (/=), (==))
import PrestoDOM
import PrestoDOM.Types.Core (class Loggable)
import Screens (ScreenName(..), getScreen)

instance showAction :: Show Action where
  show _ = ""

instance loggableAction :: Loggable Action where
  performLog action appId  = case action of
    _ -> pure unit

data Action = AfterRender
            | BackPressed
            | GenericHeaderAC GenericHeader.Action
            | SourceEditText PrimaryEditText.Action
            | DestinationEditText PrimaryEditText.Action
            | UpdateButtonAction PrimaryButton.Action
            | MyMetroTicketAction
            | ChangeTicketTab ST.TicketType
            | IncrementTicket
            | DecrementTicket
            | MetroRouteMapAction
            | ToggleTermsAndConditions

data ScreenOutput = GoBack ST.MetroTicketBookingScreenState
                  | UpdateAction ST.MetroTicketBookingScreenState
                  | MyMetroTicketScreen ST.MetroTicketBookingScreenState
                  | GoToMetroRouteMap

eval :: Action -> ST.MetroTicketBookingScreenState -> Eval Action ScreenOutput ST.MetroTicketBookingScreenState

eval BackPressed state =  exit $ GoBack state
eval (UpdateButtonAction (PrimaryButton.OnClick)) state = do
    updateAndExit state $ UpdateAction state

eval MyMetroTicketAction state = exit $ MyMetroTicketScreen state

eval IncrementTicket state = do
  if state.data.ticketCount < 6
    then continue state { data {ticketCount = state.data.ticketCount + 1 }}
    else continue state

eval DecrementTicket state = do
  if state.data.ticketCount > 1
    then continue state { data {ticketCount = state.data.ticketCount - 1 }}
    else continue state

eval MetroRouteMapAction state = exit $ GoToMetroRouteMap

eval ToggleTermsAndConditions state = continue state{props{termsAndConditionsSelected = not state.props.termsAndConditionsSelected}}

eval (ChangeTicketTab ticketType) state = continue state { data {ticketType = ticketType }}
eval _ state = continue state