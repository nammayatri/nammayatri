{-

  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.AccountSetUpScreen.Controller where

import Components.GenericHeader as GenericHeaderController
import Components.GenericRadioButton as GenericRadioButton
import Components.MenuButton as MenuButtonController
import Components.PopUpModal as PopUpModal
import Components.PrimaryButton as PrimaryButtonController
import Components.PrimaryEditText as PrimaryEditTextController
import Components.SelectListModal as SelectListModal
import Components.StepsHeaderModel.Controller as StepsHeaderModelController
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (length, trim)
import Engineering.Helpers.Commons (getNewIDWithTag)
import Helpers.Utils (clearCountDownTimer, setText)
import JBridge (hideKeyboardOnNavigation)
import Log (trackAppActionClick, trackAppEndScreen, trackAppScreenRender, trackAppBackPress, trackAppTextInput, trackAppScreenEvent)
import Prelude (class Show, bind, discard, pure, unit, not, ($), (/=), (&&), (>=), (==), (<))
import PrestoDOM (Eval, continue, continueWithCmd, exit, id, updateAndExit)
import PrestoDOM.Types.Core (class Loggable)
import Screens (ScreenName(..), getScreen)
import Screens.Types (AccountSetUpScreenState, Gender(..), ActiveFieldAccountSetup(..), ErrorType(..))
import Data.Array as DA

instance showAction :: Show Action where
  show _ = ""

instance loggableAction :: Loggable Action where
  performLog action appId  = case action of
    AfterRender -> trackAppScreenRender appId "screen" (getScreen ACCOUNT_SET_UP_SCREEN)
    BackPressed -> do
      trackAppBackPress appId (getScreen ACCOUNT_SET_UP_SCREEN)
      trackAppEndScreen appId (getScreen ACCOUNT_SET_UP_SCREEN)
    PrimaryButtonActionController act -> case act of
      PrimaryButtonController.OnClick -> do
        trackAppActionClick appId (getScreen ACCOUNT_SET_UP_SCREEN) "primary_button_action" "continue"
      PrimaryButtonController.NoAction -> trackAppActionClick appId (getScreen ACCOUNT_SET_UP_SCREEN) "primary_button" "no_action"
    NameEditTextActionController act -> case act of
      PrimaryEditTextController.TextChanged _ _ -> trackAppTextInput appId (getScreen ACCOUNT_SET_UP_SCREEN) "name_edit_text_changed" "primary_edit_text"
      PrimaryEditTextController.FocusChanged _ -> trackAppTextInput appId (getScreen ACCOUNT_SET_UP_SCREEN) "name_edit_text_focus_changed" "primary_edit_text"
    GenericHeaderActionController act -> case act of
      GenericHeaderController.PrefixImgOnClick -> do
        trackAppActionClick appId (getScreen ACCOUNT_SET_UP_SCREEN) "generic_header_action" "back_icon"
        trackAppEndScreen appId (getScreen ACCOUNT_SET_UP_SCREEN)
      GenericHeaderController.SuffixImgOnClick -> trackAppActionClick appId (getScreen ACCOUNT_SET_UP_SCREEN) "generic_header_action" "forward_icon"
    PopUpModalAction act -> case act of
      PopUpModal.OnButton1Click -> trackAppActionClick appId (getScreen ACCOUNT_SET_UP_SCREEN) "popup_modal_action" "on_goback"
      PopUpModal.OnButton2Click -> do
        trackAppActionClick appId (getScreen ACCOUNT_SET_UP_SCREEN) "popup_modal_action" "register_on_different_number"
        trackAppEndScreen appId (getScreen ACCOUNT_SET_UP_SCREEN)
      PopUpModal.NoAction -> trackAppActionClick appId (getScreen ACCOUNT_SET_UP_SCREEN) "popup_modal_action" "no_action"
      PopUpModal.OnImageClick -> trackAppActionClick appId (getScreen ACCOUNT_SET_UP_SCREEN) "popup_modal_action" "image"
      PopUpModal.ETextController act -> trackAppTextInput appId (getScreen ACCOUNT_SET_UP_SCREEN) "popup_modal_action" "primary_edit_text"
      PopUpModal.CountDown arg1 arg2 arg3 arg4 -> trackAppScreenEvent appId (getScreen ACCOUNT_SET_UP_SCREEN) "popup_modal_action" "countdown_updated"
      PopUpModal.Tipbtnclick arg1 arg2 -> trackAppScreenEvent appId (getScreen ACCOUNT_SET_UP_SCREEN) "popup_modal_action" "tip_clicked"
      PopUpModal.DismissPopup -> trackAppScreenEvent appId (getScreen ACCOUNT_SET_UP_SCREEN) "popup_modal_action" "popup_dismissed"
      PopUpModal.OnSecondaryTextClick -> trackAppScreenEvent appId (getScreen ACCOUNT_SET_UP_SCREEN) "popup_modal_action" "secondary_text_clicked"
      PopUpModal.OptionWithHtmlClick -> trackAppScreenEvent appId (getScreen ACCOUNT_SET_UP_SCREEN) "popup_modal_action" "option_with_html_clicked"
    ShowOptions -> trackAppActionClick appId (getScreen ACCOUNT_SET_UP_SCREEN) "in_screen" "show_options"
    EditTextFocusChanged -> trackAppActionClick appId (getScreen ACCOUNT_SET_UP_SCREEN) "name_edit_text_focus_changed" "edit_text"
    TextChanged value -> trackAppTextInput appId (getScreen ACCOUNT_SET_UP_SCREEN) "name_text_changed" "edit_text"
    GenderSelected value -> trackAppActionClick appId (getScreen ACCOUNT_SET_UP_SCREEN) "gender_selected" "edit_text"
    AnimationEnd _ -> trackAppActionClick appId (getScreen ACCOUNT_SET_UP_SCREEN) "show_options" "animation_end"
    StepsHeaderModelAC _ -> trackAppActionClick appId (getScreen ACCOUNT_SET_UP_SCREEN) "steps_header_modal" "backpressed"
    NameSectionClick -> trackAppActionClick appId (getScreen ACCOUNT_SET_UP_SCREEN) "in_screen" "full_name_click"
    SpecialAssistanceListAC _ -> trackAppActionClick appId (getScreen ACCOUNT_SET_UP_SCREEN) "in_screen" "special_assistance_list_click"
    GenericRadioButtonAC _ -> trackAppActionClick appId (getScreen ACCOUNT_SET_UP_SCREEN) "in_screen" "disability_radio_btn_click"
    _ -> pure unit
      


data ScreenOutput
  = GoHome AccountSetUpScreenState
  | ChangeMobileNumber

data Action
  = BackPressed
  | PrimaryButtonActionController PrimaryButtonController.Action
  | NameEditTextActionController PrimaryEditTextController.Action
  | GenericHeaderActionController GenericHeaderController.Action
  | PopUpModalAction PopUpModal.Action
  | ShowOptions
  | EditTextFocusChanged
  | TextChanged String
  | GenderSelected Gender
  | AfterRender
  | AnimationEnd String
  | StepsHeaderModelAC StepsHeaderModelController.Action
  | NameSectionClick
  | SpecialAssistanceListAC SelectListModal.Action
  | GenericRadioButtonAC GenericRadioButton.Action

eval :: Action -> AccountSetUpScreenState -> Eval Action ScreenOutput AccountSetUpScreenState
eval (PrimaryButtonActionController PrimaryButtonController.OnClick) state = do
  _ <- pure $ hideKeyboardOnNavigation true
  if state.data.disabilityOptions.activeIndex == 1 then 
    continue state{props{isSpecialAssistList = true},data{disabilityOptions{editedDisabilityReason = fromMaybe "" state.data.disabilityOptions.otherDisabilityReason}}}
    else do 
      let newState = state{data{disabilityOptions{editedDisabilityReason = "" , selectedDisability = Nothing, otherDisabilityReason = Nothing}}}
      updateAndExit newState $ GoHome newState

eval (GenericHeaderActionController (GenericHeaderController.PrefixImgOnClick)) state =
  continueWithCmd state
    [ do
        pure $ BackPressed
    ]

eval (StepsHeaderModelAC StepsHeaderModelController.OnArrowClick) state = continueWithCmd state[ do pure $ BackPressed]

eval EditTextFocusChanged state = continue state {props{genderOptionExpanded = false, activeField = Just NameSection}}

eval (GenderSelected value) state = continue state{data{gender = Just value}, props{genderOptionExpanded = false, btnActive = (state.data.name /= "") && (length state.data.name >= 3) }}

eval (TextChanged value) state = do
  let
    newState = state { data { name = trim value } ,props{ btnActive = getBtnActive state {data{name = trim value}}} }
  continue newState { data{nameErrorMessage = if (length newState.data.name >= 3) then Nothing else if (newState.data.gender /= Nothing && length newState.data.name < 3) then Just INVALID_NAME else newState.data.nameErrorMessage}, props { expandEnabled = false, genderOptionExpanded = false, isNameValid = (length newState.data.name >= 3)} }

eval (ShowOptions) state = do
  _ <- pure $ hideKeyboardOnNavigation true
  continue state{data {nameErrorMessage = if(length state.data.name >= 3) then Nothing else Just INVALID_NAME}, props{genderOptionExpanded = not state.props.genderOptionExpanded, expandEnabled = true, activeField = Just DropDown}}

eval NameSectionClick state = continue state {props{genderOptionExpanded = false, activeField = Just NameSection}}

eval (AnimationEnd _)  state = continue state{props{showOptions = false}}

eval BackPressed state = do
  if state.props.isSpecialAssistList then continue state {props{isSpecialAssistList = false}}
    else do 
      _ <- pure $ hideKeyboardOnNavigation true
      _ <- pure $ clearCountDownTimer ""
      continue state { props { backPressed = true } }

eval (PopUpModalAction (PopUpModal.OnButton1Click)) state = continue state { props { backPressed = false } }

eval (PopUpModalAction (PopUpModal.OnButton2Click)) state = exit $ ChangeMobileNumber

eval (GenericRadioButtonAC (GenericRadioButton.OnSelect idx)) state = do 
  let newState = state{data{disabilityOptions = 
                      if idx == 0 then 
                        state.data.disabilityOptions{ activeIndex = idx, specialAssistActiveIndex = 0, editedDisabilityReason = "", selectedDisability = Nothing, otherDisabilityReason = Nothing }
                        else state.data.disabilityOptions{ activeIndex = idx}
                          }}
      isBtnActive = getBtnActive newState 
  continue newState{ props{btnActive = isBtnActive }}

eval (SpecialAssistanceListAC action) state = case action of
  SelectListModal.OnGoBack -> continue state{props{isSpecialAssistList = false}}
  SelectListModal.UpdateIndex idx -> continue state{data{disabilityOptions{specialAssistActiveIndex = idx, editedDisabilityReason = fromMaybe "" state.data.disabilityOptions.otherDisabilityReason}}}
  SelectListModal.TextChanged id input -> continue state {data{disabilityOptions{ otherDisabilityReason = Just input}}}
  SelectListModal.Button2 (PrimaryButtonController.OnClick) -> do 
    _ <- pure $ hideKeyboardOnNavigation true
    let selectedDisability = (state.data.disabilityOptions.disabilityOptionList DA.!! state.data.disabilityOptions.specialAssistActiveIndex)
        selectedDisabilityTag = case selectedDisability of 
          Just disability -> disability.tag 
          Nothing -> ""
        newState = state{data{disabilityOptions{otherDisabilityReason = if selectedDisabilityTag == "OTHER" then state.data.disabilityOptions.otherDisabilityReason else Nothing , selectedDisability = selectedDisability }}}
    updateAndExit newState $ GoHome newState
  _ -> continue state

eval _ state = continue state


getBtnActive :: AccountSetUpScreenState -> Boolean
getBtnActive state = do 
  let disabilityOptions = state.data.disabilityOptions
      selectedTag  = case disabilityOptions.selectedDisability of 
                      Just disability -> Just disability.tag 
                      _ -> Nothing
      disabilityType  = case disabilityOptions.otherDisabilityReason of 
                          Just disabilityType -> disabilityType
                          _ -> ""
  (state.data.name /= "") && (length state.data.name >= 3) && (state.data.gender /= Nothing) && (if disabilityOptions.activeIndex == 1 then (if (selectedTag == Just "OTHER") then (length (disabilityType) >= 3) else true) else true)