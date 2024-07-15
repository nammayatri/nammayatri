module  Screens.HomeScreen.Controllers.PopUpModelControllers where

import Prelude
import Screens.HomeScreen.Controllers.Types
import PrestoDOM 
import Components.PopUpModal as PopUpModal
import Screens.Types 



tollChargeAmbigousPopUpAction :: PopUpModal.Action -> HomeScreenState -> Eval Action ScreenOutput HomeScreenState
tollChargeAmbigousPopUpAction action state = 
  case action of 
    PopUpModal.OnButton2Click -> continue state { data  { toll {showAmbiguousPopUp = false }}}
    _ -> update state 


tollChargeIncludedPopUpAction :: PopUpModal.Action -> HomeScreenState -> Eval Action ScreenOutput HomeScreenState
tollChargeIncludedPopUpAction action state = 
  case action of 
    PopUpModal.OnButton2Click -> continue state { data  { toll {showIncludedPopUp = false }}}
    _ -> update state