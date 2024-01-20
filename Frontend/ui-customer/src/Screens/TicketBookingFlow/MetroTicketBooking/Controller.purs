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

data ScreenOutput = GoBack ST.MetroTicketBookingScreenState

eval :: Action -> ST.MetroTicketBookingScreenState -> Eval Action ScreenOutput ST.MetroTicketBookingScreenState

eval BackPressed state =  exit $ GoBack state

eval _ state = continue state