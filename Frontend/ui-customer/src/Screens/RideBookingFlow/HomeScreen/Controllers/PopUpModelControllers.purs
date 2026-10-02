module  Screens.HomeScreen.Controllers.PopUpModelControllers where

import Prelude
import Screens.HomeScreen.Controllers.Types
import PrestoDOM 
import Components.PopUpModal as PopUpModal
import Screens.Types 
import Storage as Storage



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

intercityBusPermissionAction :: PopUpModal.Action -> HomeScreenState -> Eval Action ScreenOutput HomeScreenState
intercityBusPermissionAction action state = 
  case action of 
    PopUpModal.OnButton1Click -> do
      void $ pure $ Storage.setValueToLocalStore Storage.INTERCITY_BUS_PHONE_NUMBER_PERMISSION "false"
      continueWithCmd state{data{intercityBus{showPermissionPopUp = false, showWebView = true, hasPhoneNumberPermission = false}}} [pure IntercityBusAC]
    PopUpModal.OnButton2Click -> do 
      void $ pure $ Storage.setValueToLocalStore Storage.INTERCITY_BUS_PHONE_NUMBER_PERMISSION "true"
      continueWithCmd state{data{intercityBus{showPermissionPopUp = false, showWebView = true, hasPhoneNumberPermission = true}}} [pure IntercityBusAC]
    PopUpModal.DismissPopup -> continue state { data { intercityBus { showPermissionPopUp = false } } }
    _ -> update state