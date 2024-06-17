module Screens.BankDetailsScreen.View where

import Prelude
import Screens.BankDetailsScreen.ScreenData
import PrestoDOM
import Screens.BankDetailsScreen.Controller
import Effect



screen :: BankDetailsScreenState -> Screen Action BankDetailsScreenState ScreenOutput
screen state =
  { initialState : state
  , view
  , name : "BankDetailsScreen"
  , globalEvents : []
  , eval
  }

view:: forall w. (Action -> Effect Unit)-> BankDetailsScreenState -> PrestoDOM (Effect Unit) w
view push state = linearLayout [] []