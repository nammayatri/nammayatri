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
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import JBridge (hideKeyboardOnNavigation, requestKeyboardShow ,firebaseLogEvent)
import Log (trackAppActionClick, trackAppEndScreen, trackAppScreenRender, trackAppBackPress, trackAppTextInput, trackAppScreenEvent)
import Prelude (class Show, pure, unit, ($), discard, bind, not, (<>), (<), (==), (&&), (/=), (||), (>=))
import PrestoDOM (Eval, continue, continueWithCmd, exit, updateAndExit)
import PrestoDOM.Types.Core (class Loggable)
import Screens (ScreenName(..), getScreen)
import Screens.Types (MyProfileScreenState, DeleteStatus(..), FieldType(..), ErrorType(..), Gender(..))
import Services.API (GetProfileRes(..))
import Helpers.Utils (validateEmail)
import Data.String(length,trim)
import Storage(KeyStore(..), getValueToLocalStore)
import Engineering.Helpers.Commons(getNewIDWithTag, setText)
import Effect.Unsafe 
import Engineering.Helpers.LogEvent (logEvent)

instance showAction :: Show Action where
  show _ = ""
instance loggableAction :: Loggable Action where
   performLog action appId = case action of
    AfterRender -> trackAppScreenRender appId "screen" (getScreen MY_PROFILE_SCREEN)
    BackPressed backpressState -> pure unit
      -- trackAppBackPress appId (getScreen MY_PROFILE_SCREEN)
      -- if backpressState.props.updateProfile then trackAppScreenEvent appId (getScreen MY_PROFILE_SCREEN) "in_screen" "backpress_in_update_profile"
      --   else if backpressState.props.accountStatus == CONFIRM_REQ then trackAppScreenEvent appId (getScreen MY_PROFILE_SCREEN) "in_screen" "backpress_in_account_status_confirm"
      --     else if backpressState.props.accountStatus == DEL_REQUESTED then trackAppScreenEvent appId (getScreen MY_PROFILE_SCREEN) "in_screen" "backpress_in_account_status_delete"
      --       else trackAppEndScreen appId (getScreen MY_PROFILE_SCREEN)
    GenericHeaderActionController act -> case act of
      GenericHeader.PrefixImgOnClick -> do
        trackAppActionClick appId (getScreen MY_PROFILE_SCREEN) "generic_header_action" "back_icon"
        trackAppEndScreen appId (getScreen MY_PROFILE_SCREEN)
      GenericHeader.SuffixImgOnClick -> trackAppActionClick appId (getScreen MY_PROFILE_SCREEN) "generic_header_action" "forward_icon"
    EditProfile fieldType -> trackAppActionClick appId (getScreen MY_PROFILE_SCREEN) "primary_button" "edit"
    UpdateButtonAction act -> case act of
      PrimaryButton.OnClick -> trackAppActionClick appId (getScreen MY_PROFILE_SCREEN) "primary_button" "update"
      PrimaryButton.NoAction -> trackAppActionClick appId (getScreen MY_PROFILE_SCREEN) "primary_button" "no_action"
    UserProfile profile -> trackAppScreenEvent appId (getScreen MY_PROFILE_SCREEN) "in_screen" "user_profile"
    ShowOptions -> trackAppActionClick appId (getScreen MY_PROFILE_SCREEN) "in_screen" "open_gender_options_drop_down"
    GenderSelected value -> trackAppActionClick appId (getScreen MY_PROFILE_SCREEN) "in_screen" "gender_selected"
    NoAction -> trackAppActionClick appId (getScreen MY_PROFILE_SCREEN) "in_screen" "no_action"
    AnimationEnd _ -> trackAppActionClick appId (getScreen MY_PROFILE_SCREEN) "show_options" "animation_end"
    NameEditTextAction act -> case act of
      PrimaryEditText.TextChanged _ _ -> trackAppTextInput appId (getScreen MY_PROFILE_SCREEN) "edit_name_text_changed" "primary_edit_text"
      PrimaryEditText.FocusChanged _ -> trackAppTextInput appId (getScreen MY_PROFILE_SCREEN) "edit_name_text_focus_changed" "primary_edit_text"
    EmailIDEditTextAction act -> case act of
      PrimaryEditText.TextChanged _ _ -> trackAppActionClick appId (getScreen MY_PROFILE_SCREEN) "edit_email_text_changed" "primary_edit_text"
      PrimaryEditText.FocusChanged _ -> trackAppActionClick appId (getScreen MY_PROFILE_SCREEN) "edit_email_text_focus_changed" "primary_edit_text"
data Action = GenericHeaderActionController GenericHeader.Action
            | BackPressed MyProfileScreenState
            | AfterRender
            | EditProfile (Maybe FieldType)
            | NameEditTextAction PrimaryEditText.Action
            | EmailIDEditTextAction PrimaryEditText.Action
            | UpdateButtonAction PrimaryButton.Action
            | UserProfile GetProfileRes
            | ShowOptions
            | GenderSelected Gender
            | NoAction
            | AnimationEnd String
data ScreenOutput = GoToHomeScreen | UpdateProfile MyProfileScreenState | GoToHome
eval :: Action -> MyProfileScreenState -> Eval Action ScreenOutput MyProfileScreenState
eval (GenericHeaderActionController (GenericHeader.PrefixImgOnClick)) state = continueWithCmd state [do pure $ BackPressed state]
eval (BackPressed backpressState) state = do
  if state.props.updateProfile && state.props.fromHomeScreen then do
    _ <- pure $ hideKeyboardOnNavigation true
    exit $ GoToHomeScreen
    else if state.props.updateProfile then do
      _ <- pure $ hideKeyboardOnNavigation true
      continue state { props { updateProfile = false, genderOptionExpanded = false , expandEnabled = false, isEmailValid = true} }
        else exit $ GoToHomeScreen
eval (EditProfile fieldType) state = do
  case fieldType of
    Just EMAILID_ -> do
      _ <- pure $ requestKeyboardShow (getNewIDWithTag "EmailEditText")
      _ <- pure $ setText (getNewIDWithTag "EmailEditText") (fromMaybe "" state.data.emailId)
      pure unit
    _ -> pure unit
  continue state { props { isBtnEnabled = false , updateProfile = true , isEmailValid = true}, data { editedName = state.data.name, editedEmailId = state.data.emailId, editedGender = state.data.gender} }
eval ShowOptions state = do
  _ <- pure $ hideKeyboardOnNavigation true
  continue state{props{genderOptionExpanded = not state.props.genderOptionExpanded, showOptions = true, expandEnabled = true}}
eval ( AnimationEnd _ )state = continue state{props{showOptions = false}}
eval (GenderSelected value) state = do
    continue state{data{editedGender = Just value}, props{genderOptionExpanded = false , isBtnEnabled = (( length (trim state.data.editedName) >=3)  && state.props.isEmailValid)}}
eval (UserProfile (GetProfileRes profile)) state = do
  let middleName = case profile.middleName of
                    Just ""  -> ""
                    Just name -> (" " <> name)
                    Nothing -> ""
      lastName   = case profile.lastName of
                    Just "" -> ""
                    Just name -> (" " <> name)
                    Nothing -> ""
      name = (fromMaybe "" profile.firstName) <> middleName <> lastName
      gender = case (profile.gender) of
        Just "MALE" -> Just MALE
        Just "FEMALE" -> Just FEMALE
        Just "OTHER" -> Just OTHER
        Just "PREFER_NOT_TO_SAY" -> Just PREFER_NOT_TO_SAY
        _ -> Nothing
  continue state { data { name = name, editedName = name, gender = gender, emailId = profile.email} }
eval (NameEditTextAction (PrimaryEditText.TextChanged id value)) state = do
  if (length (trim value) < 3) then continue state {data {editedName = value, nameErrorMessage = checkError "name" (Just state.data.name) value}, props{isNameValid = false, isBtnEnabled = false }}
    else do 
      let isBtnActive = state.data.emailErrorMessage == Nothing && state.props.isEmailValid && ((trim value) /= state.data.name) ||(state.data.emailId /= state.data.editedEmailId)
      continue state { data { editedName = trim value, nameErrorMessage = Nothing }, props{isNameValid = true, isBtnEnabled = isBtnActive} }
eval (EmailIDEditTextAction (PrimaryEditText.TextChanged id value)) state = do
  let isButtonActive = state.data.nameErrorMessage == Nothing && checkValid state.data.emailId value && (state.data.emailId /= Just value || state.data.name /= state.data.editedName)
  if (state.data.emailId == Nothing) then  continue state {  data { editedEmailId = Just value, emailErrorMessage = checkError "email" state.data.emailId value}
                    , props { isEmailValid = checkValid state.data.emailId value, isBtnEnabled = isButtonActive}}
    else if (value == "" && state.data.emailErrorMessage == Just EMAIL_EXISTS) then 
      continue state{props  { isEmailValid = false, isBtnEnabled = false, genderOptionExpanded = state.props.fromHomeScreen, expandEnabled = state.props.fromHomeScreen}}
    else continue state { data {editedEmailId = Just value , emailErrorMessage = checkError "email" state.data.emailId value }
                        , props{isEmailValid = checkValid state.data.emailId value, 
                          isBtnEnabled =  isButtonActive && (length (trim state.data.editedName) >=3) , genderOptionExpanded = false}}
eval (UpdateButtonAction (PrimaryButton.OnClick)) state = do
  _ <- pure $ hideKeyboardOnNavigation true
  if state.data.gender /= state.data.editedGender then do
      let _ = unsafePerformEffect $ logEvent state.data.logField $ if state.props.fromHomeScreen then "banner_gender_selected" else "profile_gender_selected"
      pure unit
    else pure unit
  updateAndExit state $ UpdateProfile state
eval _ state = continue state


checkError :: String -> Maybe String -> String -> Maybe ErrorType
checkError inputType originalValue value = case inputType of
                                      "email" ->  if (length value == 0 && originalValue /= Nothing) then Just EMAIL_CANNOT_BE_BLANK
                                                    else if ((length value == 0 && originalValue == Nothing) || (validateEmail value)) then Nothing
                                                    else Just INVALID_EMAIL
                                      "name"  ->  if (length ( trim value ) == 0) then Just NAME_CANNOT_BE_BLANK
                                                    else if (length ( trim value)  < 3) then Just INVALID_NAME
                                                    else Nothing
                                      _ -> Nothing

checkValid :: Maybe String -> String -> Boolean
checkValid originalValue value = if (length value == 0 && originalValue == Nothing) then true
                                    else if (length value == 0) then false
                                    else validateEmail value