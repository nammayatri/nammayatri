module Screens.DeleteAccountScreen.Controller where

import Screens.DeleteAccountScreen.ScreenData
import PrestoDOM
import Prelude
import Data.Generic.Rep (class Generic)
import Components.PopUpModal as PopUpModal
import Components.GenericHeader as GenericHeader
import Components.PrimaryButton as PrimaryButton
import Components.PrimaryEditText as PrimaryEditText
import Data.String

data ScreenOutput = Submit State | Back State

data Action = NextClick | BackClick | DeleteGenericHeaderAC GenericHeader.Action
  | PrimaryButtonAC PrimaryButton.Action
  | EmailEditTextAC PrimaryEditText.Action
  | DescriptionEditTextAC PrimaryEditText.Action
  | PopUpModalAction PopUpModal.Action
  | RSPopUpModalAction PopUpModal.Action

instance showAction :: Show Action where 
  show _ = ""

instance loggableAction :: Loggable Action where
    performLog = defaultPerformLog

eval :: Action -> State -> Eval Action ScreenOutput State
eval (EmailEditTextAC (PrimaryEditText.TextChanged _ a)) state = continue state{data {email = trim(a)}}

eval (DescriptionEditTextAC (PrimaryEditText.TextChanged id a)) state = continue state{data {description = a}}

eval (DeleteGenericHeaderAC (GenericHeader.PrefixImgOnClick )) state = exit $ Back state
eval (PrimaryButtonAC (PrimaryButton.OnClick)) state =  continue state{props {showConfirmPopUp = true}}

eval (PopUpModalAction (PopUpModal.OnButton1Click)) state = continue state {props{ showConfirmPopUp= false}}
eval (PopUpModalAction (PopUpModal.OnButton2Click)) state = continue state {props{ showConfirmPopUp= false, showRequestSubmitted = true}}
eval (RSPopUpModalAction (PopUpModal.OnButton1Click)) state = exit $ Back state
eval (RSPopUpModalAction (PopUpModal.OnButton2Click)) state = exit $ Back state

eval _ state = continue state