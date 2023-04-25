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
import Data.Maybe (Maybe(..), fromMaybe)
import JBridge (hideKeyboardOnNavigation, requestKeyboardShow ,firebaseLogEvent)
import Log (trackAppActionClick, trackAppEndScreen, trackAppScreenRender, trackAppBackPress, trackAppTextInput, trackAppScreenEvent)
import Prelude (class Show, pure, unit, ($), discard, bind, not, (<>), (==), (&&), (/=), (||))
import PrestoDOM (Eval, continue, continueWithCmd, exit, updateAndExit)
import PrestoDOM.Types.Core (class Loggable)
import Screens (ScreenName(..), getScreen)
import Screens.Types (MyProfileScreenState, DeleteStatus(..), FieldType(..), EmailErrorType(..), Gender(..))
import Services.API (GetProfileRes(..))
import Helpers.Utils (validateEmail)
import Data.String(length)
import Storage(KeyStore(..), getValueToLocalStore)
import Engineering.Helpers.Commons(getNewIDWithTag)
import Debug.Trace(spy)
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
    EditProfile fieldType -> trackAppActionClick appId (getScreen MY_PROFILE_SCREEN) "primary_button" "edit"
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
      PopUpModal.ETextController act -> trackAppTextInput appId (getScreen MY_PROFILE_SCREEN) "popup_modal_action" "primary_edit_text_delete_account"
      PopUpModal.CountDown arg1 arg2 arg3 arg4 -> trackAppScreenEvent appId (getScreen MY_PROFILE_SCREEN) "popup_modal_action" "countdown_updated"
      PopUpModal.Tipbtnclick arg1 arg2 -> trackAppScreenEvent appId (getScreen MY_PROFILE_SCREEN) "popup_modal_action" "tip_clicked"
      PopUpModal.DismissPopup -> trackAppScreenEvent appId (getScreen MY_PROFILE_SCREEN) "popup_modal_action" "popup_dismissed"
    AccountDeletedModalAction act -> case act of
      PopUpModal.OnButton1Click -> do
        trackAppActionClick appId (getScreen MY_PROFILE_SCREEN) "popup_modal_action" "account_deleted"
        trackAppEndScreen appId (getScreen MY_PROFILE_SCREEN)
      PopUpModal.OnButton2Click -> trackAppActionClick appId (getScreen MY_PROFILE_SCREEN) "popup_modal_action" "button_2"
      PopUpModal.NoAction -> trackAppActionClick appId (getScreen MY_PROFILE_SCREEN) "popup_modal_action" "no_action"
      PopUpModal.OnImageClick -> trackAppActionClick appId (getScreen MY_PROFILE_SCREEN) "popup_modal_action" "image"
      PopUpModal.ETextController act -> trackAppTextInput appId (getScreen MY_PROFILE_SCREEN) "popup_modal_action" "primary_edit_text_account_deleted"
      PopUpModal.CountDown arg1 arg2 arg3 arg4 -> trackAppScreenEvent appId (getScreen MY_PROFILE_SCREEN) "popup_modal_action" "countdown_updated"
      PopUpModal.Tipbtnclick arg1 arg2 -> trackAppScreenEvent appId (getScreen MY_PROFILE_SCREEN) "popup_modal_action" "tip_clicked"
      PopUpModal.DismissPopup -> trackAppScreenEvent appId (getScreen MY_PROFILE_SCREEN) "popup_modal_action" "popup_dismissed"
    ReqDelAccount -> trackAppActionClick appId (getScreen MY_PROFILE_SCREEN) "in_screen" "delete_account_request"
    ShowOptions -> trackAppActionClick appId (getScreen MY_PROFILE_SCREEN) "in_screen" "open_gender_options_drop_down"
    GenderSelected value -> trackAppActionClick appId (getScreen MY_PROFILE_SCREEN) "in_screen" "gender_selected"
    EmailIDEditTextAction (PrimaryEditText.TextChanged id value) -> trackAppActionClick appId (getScreen MY_PROFILE_SCREEN) "edit_email_text_changed" "primary_edit_text"
    NoAction -> trackAppActionClick appId (getScreen MY_PROFILE_SCREEN) "in_screen" "no_action"
    AnimationEnd _ -> trackAppActionClick appId (getScreen MY_PROFILE_SCREEN) "show_options" "animation_end"
data Action = GenericHeaderActionController GenericHeader.Action
            | BackPressed MyProfileScreenState
            | AfterRender
            | EditProfile (Maybe FieldType)
            | NameEditTextAction PrimaryEditText.Action
            | EmailIDEditTextAction PrimaryEditText.Action
            | UpdateButtonAction PrimaryButton.Action
            | UserProfile GetProfileRes
            | PopUpModalAction PopUpModal.Action
            | AccountDeletedModalAction PopUpModal.Action
            | ReqDelAccount
            | ShowOptions
            | GenderSelected Gender
            | NoAction
            | AnimationEnd String
data ScreenOutput = GoToHomeScreen | UpdateProfile MyProfileScreenState | DeleteAccount MyProfileScreenState | GoToHome
eval :: Action -> MyProfileScreenState -> Eval Action ScreenOutput MyProfileScreenState
eval (GenericHeaderActionController (GenericHeader.PrefixImgOnClick)) state = continueWithCmd state [do pure $ BackPressed state]
eval (BackPressed backpressState) state = do
  if state.props.updateProfile && state.props.fromHomeScreen then do
    _ <- pure $ hideKeyboardOnNavigation true
    exit $ GoToHomeScreen
    else if state.props.updateProfile then do
      _ <- pure $ hideKeyboardOnNavigation true
      continue state { props { updateProfile = false, genderOptionExpanded = false , expandEnabled = false, isEmailValid = true} }
      else if state.props.accountStatus == CONFIRM_REQ then
        continue state{props{accountStatus = ACTIVE}}
        else if state.props.accountStatus == DEL_REQUESTED then
        continue state
        else exit $ GoToHomeScreen
eval (EditProfile fieldType) state = do
  case fieldType of
    Just EMAILID_ -> do
      _ <- pure $ requestKeyboardShow (getNewIDWithTag "EmailEditText")
      pure unit
    _ -> pure unit
  continue state { props { updateProfile = true , isEmailValid = true}, data { editedName = state.data.name, editedEmailId = state.data.emailId, editedGender = state.data.gender} }
eval ShowOptions state = do
  _ <- pure $ hideKeyboardOnNavigation true
  continue state{props{genderOptionExpanded = not state.props.genderOptionExpanded, showOptions = true, expandEnabled = true}}
eval ( AnimationEnd _ )state = continue state{props{showOptions = false}}
eval (GenderSelected value) state = continue state{data{editedGender = Just value}, props{genderOptionExpanded = false}}
eval (UserProfile (GetProfileRes profile)) state = do
  let name = (fromMaybe "" profile.firstName) <> " " <> (fromMaybe "" profile.middleName) <> " " <> (fromMaybe "" profile.lastName)
      gender = case (profile.gender) of
        Just "MALE" -> Just MALE
        Just "FEMALE" -> Just FEMALE
        Just "OTHER" -> Just OTHER
        Just "PREFER_NOT_TO_SAY" -> Just PREFER_NOT_TO_SAY
        _ -> Nothing
  continue state { data { name = name, editedName = name, gender = gender, emailId = profile.email } }
eval (NameEditTextAction (PrimaryEditText.TextChanged id value)) state = do 
  _ <- pure $ spy "Value changed"  value 
  continue state { data { editedName = value }, props{genderOptionExpanded = false, expandEnabled = false} }
eval (EmailIDEditTextAction (PrimaryEditText.TextChanged id value)) state = do
  if (value == "" && state.data.errorMessage == Just EMAIL_EXISTS) then continue state {props{ isEmailValid = false, isBtnEnabled = false, genderOptionExpanded = false, expandEnabled = false}}
    else continue state {data {editedEmailId = Just value , errorMessage = if (length value == 0) then Nothing else if ( validateEmail value) then Nothing else Just INVALID_EMAIL },props{ isEmailValid = if (length value == 0) then true else validateEmail value, isBtnEnabled = if (length value == 0) then true else validateEmail value, genderOptionExpanded = false}}
eval (UpdateButtonAction (PrimaryButton.OnClick)) state = do
  _ <- pure $ hideKeyboardOnNavigation true
  if state.data.gender /= state.data.editedGender then do
      _ <- pure $ firebaseLogEvent if state.props.fromHomeScreen then "banner_gender_selected" else "profile_gender_selected"
      pure unit
    else pure unit
  updateAndExit state $ UpdateProfile state
eval ReqDelAccount state = continue state{props{accountStatus = CONFIRM_REQ}}
eval (PopUpModalAction (PopUpModal.OnButton1Click)) state = continue state {props{ accountStatus= ACTIVE}}
eval (PopUpModalAction (PopUpModal.OnButton2Click)) state = exit $ DeleteAccount state
eval (AccountDeletedModalAction (PopUpModal.OnButton1Click)) state =  updateAndExit (state {props{accountStatus = ACTIVE}} ) $ GoToHome
eval _ state = continue state