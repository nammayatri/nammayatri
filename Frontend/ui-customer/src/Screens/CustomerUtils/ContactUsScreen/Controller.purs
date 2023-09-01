{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.ContactUsScreen.Controller where

import Components.GenericHeader as GenericHeader
import Components.PrimaryButton as PrimaryButton
import Components.PrimaryEditText as PrimaryEditText
import Data.String (length, trim)
import Helpers.Utils (validateEmail)
import JBridge (hideKeyboardOnNavigation)
import Log (printLog)
import Log (trackAppActionClick, trackAppEndScreen, trackAppScreenRender, trackAppBackPress, trackAppTextInput, trackAppScreenEvent)
import Prelude (pure, unit, class Show, bind, (>), (&&), ($), discard, (==))
import PrestoDOM (Eval, continue, continueWithCmd, exit, updateAndExit)
import PrestoDOM.Types.Core (class Loggable)
import Screens (ScreenName(..), getScreen)
import Screens.Types (ContactUsScreenState, ErrorType(..))
import Storage (KeyStore(..), setValueToLocalStore)
import Data.Maybe(Maybe(..))
import Effect.Unsafe (unsafePerformEffect)
import Engineering.Helpers.LogEvent (logEvent)

instance showAction :: Show Action where
    show _ = ""

instance loggableAction :: Loggable Action where
    performLog action appId = case action of
        AfterRender -> trackAppScreenRender appId "screen" (getScreen CONTACT_US_SCREEN)
        BackPressed -> do
            trackAppBackPress appId (getScreen CONTACT_US_SCREEN)
            trackAppEndScreen appId (getScreen CONTACT_US_SCREEN)
        GenericHeaderActionController act -> case act of
            GenericHeader.PrefixImgOnClick -> do
                trackAppActionClick appId (getScreen CONTACT_US_SCREEN) "generic_header_action" "back_icon"
                trackAppEndScreen appId (getScreen CONTACT_US_SCREEN)
            GenericHeader.SuffixImgOnClick -> trackAppActionClick appId (getScreen CONTACT_US_SCREEN) "generic_header_action" "forward_icon"
        GoToHome -> do
            trackAppActionClick appId (getScreen CONTACT_US_SCREEN) "in_screen" "go_to_home"
            trackAppEndScreen appId (getScreen CONTACT_US_SCREEN)
        PrimaryButtonActionController act -> case act of 
            PrimaryButton.OnClick -> do
                trackAppActionClick appId (getScreen CONTACT_US_SCREEN) "primary_button_action" "go_to_home/submit"
                trackAppEndScreen appId (getScreen CONTACT_US_SCREEN)
            PrimaryButton.NoAction -> trackAppActionClick appId (getScreen CONTACT_US_SCREEN) "primary_button" "no_action"
        SubjectEditTextActionController act -> case act of
            PrimaryEditText.TextChanged _ _ -> trackAppTextInput appId (getScreen CONTACT_US_SCREEN) "subject_edit_text_changed" "primary_edit_text" 
            PrimaryEditText.FocusChanged _ -> trackAppTextInput appId (getScreen CONTACT_US_SCREEN) "subject_edit_text_focus_changed" "primary_edit_text" 
        EmailEditTextActionController act -> case act of
            PrimaryEditText.TextChanged _ _ -> trackAppTextInput appId (getScreen CONTACT_US_SCREEN) "email_edit_text_changed" "primary_edit_text"
            PrimaryEditText.FocusChanged _ -> trackAppTextInput appId (getScreen CONTACT_US_SCREEN) "email_edit_text_focus_changed" "primary_edit_text" 
        DescriptionEditTextActionController act -> case act of
            PrimaryEditText.TextChanged _ _ -> trackAppTextInput appId (getScreen CONTACT_US_SCREEN) "description_edit_text_changed" "primary_edit_text"
            PrimaryEditText.FocusChanged _ -> trackAppTextInput appId (getScreen CONTACT_US_SCREEN) "description_edit_text_focus_changed" "primary_edit_text" 

data ScreenOutput = GoBack | GoHome ContactUsScreenState

data Action = GoToHome 
            | BackPressed 
            | PrimaryButtonActionController PrimaryButton.Action 
            | SubjectEditTextActionController PrimaryEditText.Action 
            | EmailEditTextActionController PrimaryEditText.Action 
            | DescriptionEditTextActionController PrimaryEditText.Action 
            | GenericHeaderActionController GenericHeader.Action
            | AfterRender 

eval :: Action -> ContactUsScreenState -> Eval Action ScreenOutput ContactUsScreenState

eval BackPressed state = exit GoBack

eval GoToHome state = exit $ GoHome state

eval (GenericHeaderActionController (GenericHeader.PrefixImgOnClick )) state = continueWithCmd state [do pure BackPressed]

eval (PrimaryButtonActionController (PrimaryButton.OnClick)) state = do
    _ <- pure $ hideKeyboardOnNavigation true
    let _ = unsafePerformEffect $ logEvent state.data.logField "ny_user_write_to_us"
    if (state.props.isSubmitted) then updateAndExit state  $  GoHome state
        else continue state{props{isSubmitted = true}}

eval (SubjectEditTextActionController (PrimaryEditText.TextChanged id a)) state = continue state{data {subject = trim(a)}, props{btnActive = if ((length (trim(a))) > 0 && (length state.data.email) > 0 && (length state.data.description > 0 && (validateEmail state.data.email))) then true else false}}

eval (EmailEditTextActionController (PrimaryEditText.TextChanged id a)) state = continue state{data {email = trim(a), errorMessage = if (length a == 0) then Nothing else if ( validateEmail a) then Nothing else Just INVALID_EMAIL },props{btnActive = if ((length state.data.subject > 0 )&& (length (trim(a)) > 0 )&& (length state.data.description > 0) && (validateEmail a)) then true else false}}

eval (DescriptionEditTextActionController (PrimaryEditText.TextChanged id a)) state = continue state{data {description = a},props{btnActive = if ((length state.data.subject > 0 )&& (length state.data.email > 0) && (length a > 0) && (validateEmail state.data.email)) then true else false}}
    

eval _ state = continue state