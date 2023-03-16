{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.MyProfileScreen.Controller where

import Components.GenericHeader as GenericHeader
import Components.PopUpModal as PopUpModal
import Components.PrimaryButton as PrimaryButton
import Components.PrimaryEditText as PrimaryEditText
import Data.Maybe (fromMaybe)
import JBridge (hideKeyboardOnNavigation)
import Log (trackAppActionClick, trackAppEndScreen, trackAppScreenRender, trackAppBackPress, trackAppTextInput, trackAppScreenEvent)
import Prelude (class Show, pure, unit, ($), discard, bind, (<>), (==))
import PrestoDOM (Eval, continue, continueWithCmd, exit, updateAndExit)
import PrestoDOM.Types.Core (class Loggable)
import Screens (ScreenName(..), getScreen)
import Screens.Types (MyProfileScreenState, DeleteStatus(..))
import Services.API (GetProfileRes(..))

instance showAction :: Show Action where
  show _ = ""

instance loggableAction :: Loggable Action where
   performLog action appId = case action of
    AfterRender -> trackAppScreenRender appId "screen" (getScreen MY_PROFILE_SCREEN)
    BackPressed backpressState -> do
      trackAppBackPress appId (getScreen MY_PROFILE_SCREEN)
      if backpressState.props.updateProfile then trackAppScreenEvent appId (getScreen MY_PROFILE_SCREEN) "in_screen" "backpress_in_update_profile"
        else if backpressState.props.accountStatus == CONFIRM_REQ then trackAppScreenEvent appId (getScreen MY_PROFILE_SCREEN) "in_screen" "backpress_in_account_status_confirm"
          else if backpressState.props.accountStatus == DEL_REQUESTED then trackAppScreenEvent appId (getScreen MY_PROFILE_SCREEN) "in_screen" "backpress_in_account_status_delete"
            else trackAppEndScreen appId (getScreen MY_PROFILE_SCREEN)
    GenericHeaderActionController act -> case act of
      GenericHeader.PrefixImgOnClick -> do
        trackAppActionClick appId (getScreen MY_PROFILE_SCREEN) "generic_header_action" "back_icon"
        trackAppEndScreen appId (getScreen MY_PROFILE_SCREEN)
      GenericHeader.SuffixImgOnClick -> trackAppActionClick appId (getScreen MY_PROFILE_SCREEN) "generic_header_action" "forward_icon"
    EditProfile -> trackAppActionClick appId (getScreen MY_PROFILE_SCREEN) "primary_button" "edit"
    NameEditTextAction (PrimaryEditText.TextChanged id value) -> trackAppTextInput appId (getScreen MY_PROFILE_SCREEN) "edit_name_text_changed" "primary_edit_text"
    UpdateButtonAction act -> case act of
      PrimaryButton.OnClick -> trackAppActionClick appId (getScreen MY_PROFILE_SCREEN) "primary_button" "update"
      PrimaryButton.NoAction -> trackAppActionClick appId (getScreen MY_PROFILE_SCREEN) "primary_button" "no_action"
    UserProfile profile -> trackAppScreenEvent appId (getScreen MY_PROFILE_SCREEN) "in_screen" "user_profile"
    PopUpModalAction act -> case act of
      PopUpModal.OnButton1Click -> trackAppActionClick appId (getScreen MY_PROFILE_SCREEN) "popup_modal_action" "delete_account_cancel"
      PopUpModal.OnButton2Click -> do
        trackAppActionClick appId (getScreen MY_PROFILE_SCREEN) "popup_modal_action" "delete_account_accept"
        trackAppEndScreen appId (getScreen MY_PROFILE_SCREEN)
      PopUpModal.NoAction -> trackAppActionClick appId (getScreen MY_PROFILE_SCREEN) "popup_modal_action" "no_action"
      PopUpModal.OnImageClick -> trackAppActionClick appId (getScreen MY_PROFILE_SCREEN) "popup_modal_action" "image"
      PopUpModal.ETextController act -> trackAppTextInput appId (getScreen MY_PROFILE_SCREEN) "popup_modal_action" "primary_edit_text"
      PopUpModal.CountDown arg1 arg2 arg3 arg4 -> trackAppScreenEvent appId (getScreen MY_PROFILE_SCREEN) "popup_modal_action" "countdown_updated"
    AccountDeletedModalAction act -> case act of
      PopUpModal.OnButton1Click -> do
        trackAppActionClick appId (getScreen MY_PROFILE_SCREEN) "popup_modal_action" "account_deleted"
        trackAppEndScreen appId (getScreen MY_PROFILE_SCREEN)
      PopUpModal.OnButton2Click -> trackAppActionClick appId (getScreen MY_PROFILE_SCREEN) "popup_modal_action" "button_2"
      PopUpModal.NoAction -> trackAppActionClick appId (getScreen MY_PROFILE_SCREEN) "popup_modal_action" "no_action"
      PopUpModal.OnImageClick -> trackAppActionClick appId (getScreen MY_PROFILE_SCREEN) "popup_modal_action" "image"
      PopUpModal.ETextController act -> trackAppTextInput appId (getScreen MY_PROFILE_SCREEN) "popup_modal_action" "primary_edit_text"
      PopUpModal.CountDown arg1 arg2 arg3 arg4 -> trackAppScreenEvent appId (getScreen MY_PROFILE_SCREEN) "popup_modal_action" "countdown_updated"
    ReqDelAccount -> trackAppActionClick appId (getScreen MY_PROFILE_SCREEN) "in_screen" "delete_account_request"
    
data Action = GenericHeaderActionController GenericHeader.Action
            | BackPressed MyProfileScreenState
            | AfterRender
            | EditProfile
            | NameEditTextAction PrimaryEditText.Action
            | UpdateButtonAction PrimaryButton.Action
            | UserProfile GetProfileRes
            | PopUpModalAction PopUpModal.Action
            | AccountDeletedModalAction PopUpModal.Action
            | ReqDelAccount

data ScreenOutput = GoToHomeScreen | UpdateProfile MyProfileScreenState | DeleteAccount MyProfileScreenState | GoToHome
eval :: Action -> MyProfileScreenState -> Eval Action ScreenOutput MyProfileScreenState

eval (GenericHeaderActionController (GenericHeader.PrefixImgOnClick)) state = continueWithCmd state [do pure $ BackPressed state]

eval (BackPressed backpressState) state = do 
  if state.props.updateProfile then 
    continue state { props { updateProfile = false } }
    else if state.props.accountStatus == CONFIRM_REQ then
      continue state{props{accountStatus = ACTIVE}}
      else if state.props.accountStatus == DEL_REQUESTED then
      continue state
      else exit $ GoToHomeScreen

eval EditProfile state = continue state { props { updateProfile = true }, data { editedName = state.data.name} }

eval (UserProfile (GetProfileRes profile)) state = do 
  let name = (fromMaybe "" profile.firstName) <> " " <> (fromMaybe "" profile.middleName) <> " " <> (fromMaybe "" profile.lastName)
  continue state { data { name = name } }

eval (NameEditTextAction (PrimaryEditText.TextChanged id value)) state = continue state { data { editedName = value } }

eval (UpdateButtonAction (PrimaryButton.OnClick)) state = do 
  _ <- pure $ hideKeyboardOnNavigation true
  updateAndExit state $ UpdateProfile state{data{name = state.data.editedName}}

eval ReqDelAccount state = continue state{props{accountStatus = CONFIRM_REQ}}
eval (PopUpModalAction (PopUpModal.OnButton1Click)) state = continue state {props{ accountStatus= ACTIVE}}
eval (PopUpModalAction (PopUpModal.OnButton2Click)) state = exit $ DeleteAccount state
eval (AccountDeletedModalAction (PopUpModal.OnButton1Click)) state =  updateAndExit (state {props{accountStatus = ACTIVE}} ) $ GoToHome 

eval _ state = continue state