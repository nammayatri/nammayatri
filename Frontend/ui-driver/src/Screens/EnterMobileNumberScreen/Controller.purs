{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.EnterMobileNumberScreen.Controller where
import Prelude (class Show, not, pure, unit, (&&), (<=), (==), (||), discard, bind, ($), (>), (<))
import PrestoDOM (Eval, update, continue, continueWithCmd, exit)
import Screens.Types (EnterMobileNumberScreenState)
import Components.PrimaryEditText.Controllers as PrimaryEditText
import Components.MobileNumberEditor as MobileNumberEditor
import Components.PrimaryButton.Controller as PrimaryButton
import PrestoDOM.Types.Core (class Loggable)
import Data.String (length)
import Data.String.CodeUnits (charAt)
import Data.Maybe (Maybe(..))
import JBridge (requestKeyboardShow,hideKeyboardOnNavigation)
import Engineering.Helpers.Commons (getNewIDWithTag)
import Effect.Class (liftEffect)
import Log (trackAppActionClick, trackAppEndScreen, trackAppScreenRender, trackAppBackPress, trackAppTextInput, trackAppScreenEvent)
import Screens (ScreenName(..), getScreen)
import Effect.Unsafe (unsafePerformEffect)
import Engineering.Helpers.LogEvent (logEvent)
import ConfigProvider

instance showAction :: Show Action where
  show _ = ""

instance loggableAction :: Loggable Action where
  performLog action appId = case action of
    AfterRender -> trackAppScreenRender appId "screen" (getScreen ENTER_MOBILE_NUMBER_SCREEN)
    BackPressed -> do
      trackAppBackPress appId (getScreen ENTER_MOBILE_NUMBER_SCREEN)
      trackAppEndScreen appId (getScreen ENTER_MOBILE_NUMBER_SCREEN)
    PrimaryEditTextAction act -> case act of
      MobileNumberEditor.TextChanged id value -> trackAppTextInput appId (getScreen ENTER_MOBILE_NUMBER_SCREEN) "mobilenumber_edit_text_changed" "primary_edit_text"
      MobileNumberEditor.FocusChanged _ -> trackAppTextInput appId (getScreen ENTER_MOBILE_NUMBER_SCREEN) "mobilenumber_edit_text_focus_changed" "primary_edit_text"
      MobileNumberEditor.CountryCodeSelected _ -> trackAppTextInput appId (getScreen ENTER_MOBILE_NUMBER_SCREEN) "countrycode_edit_text_changed" "on_click_country_code"
      MobileNumberEditor.ShowOptions -> trackAppTextInput appId (getScreen ENTER_MOBILE_NUMBER_SCREEN) "country_code_list_showed" "on_click_show"
      MobileNumberEditor.CloseOptions -> trackAppTextInput appId (getScreen ENTER_MOBILE_NUMBER_SCREEN) "country_code_list_closed" "on_click_close"
    PrimaryButtonActionController act -> case act of
      PrimaryButton.OnClick -> do
        trackAppActionClick appId (getScreen ENTER_MOBILE_NUMBER_SCREEN) "primary_button" "next_on_click"
        trackAppEndScreen appId (getScreen ENTER_MOBILE_NUMBER_SCREEN)
      PrimaryButton.NoAction -> trackAppActionClick appId (getScreen ENTER_MOBILE_NUMBER_SCREEN) "primary_button" "no_action"
    NonDisclosureAgreementAction -> trackAppActionClick appId (getScreen ENTER_MOBILE_NUMBER_SCREEN) "in_screen" "no_discloure_agreement"
    CheckBoxClicked -> trackAppScreenEvent appId (getScreen ENTER_MOBILE_NUMBER_SCREEN) "in_screen" "checkbox_clicked"
    CheckClickability -> trackAppScreenEvent appId (getScreen ENTER_MOBILE_NUMBER_SCREEN) "in_screen" "check_clickability"
    NoAction -> trackAppScreenEvent appId (getScreen ENTER_MOBILE_NUMBER_SCREEN) "in_screen" "no_action"
    _ -> trackAppScreenEvent appId (getScreen ENTER_MOBILE_NUMBER_SCREEN) "in_screen" "no_action"

data ScreenOutput = GoBack | GoToNextScreen EnterMobileNumberScreenState
data Action = BackPressed 
            | PrimaryEditTextAction MobileNumberEditor.Action
            | PrimaryButtonActionController PrimaryButton.Action
            | NoAction
            | CheckBoxClicked
            | CheckClickability
            | AfterRender
            | NonDisclosureAgreementAction

eval :: Action -> EnterMobileNumberScreenState -> Eval Action ScreenOutput EnterMobileNumberScreenState
eval AfterRender state = continue state
eval BackPressed state = do
        pure $ hideKeyboardOnNavigation true
        exit GoBack
eval (PrimaryButtonActionController (PrimaryButton.OnClick)) state = exit (GoToNextScreen state)
eval (PrimaryEditTextAction (MobileNumberEditor.TextChanged valId newVal)) state = do
  _ <- if length newVal == 10 then do
            pure $ hideKeyboardOnNavigation true 
            else pure unit    
  let config = getAppConfig appConfig
      isValidMobileNumber = if config.allowAllMobileNumber then true
                              else case (charAt 0 newVal) of 
                                Just a -> if a=='0' || a=='1' || a=='2' || a=='5' then false 
                                            else if a=='3' || a=='4' then
                                                if newVal=="4000400040" || newVal=="3000300030" || newVal=="5000500050" then true else false 
                                                    else true 
                                Nothing -> true
  if (length newVal == 10 && isValidMobileNumber) then do 
    let _ = unsafePerformEffect $ logEvent state.data.logField "ny_driver_mobnum_entry"
    pure unit
    else pure unit
  continue  state { props = state.props { btnActive = if (length newVal == 10 && isValidMobileNumber) then true else false
                                        , isValid = not isValidMobileNumber
                                        , mobileNumberEditFocused = length newVal < 10 }
                                        , data = state.data { mobileNumber = if length newVal <= 10 then newVal else state.data.mobileNumber}}

eval (PrimaryEditTextAction (MobileNumberEditor.FocusChanged focusChanged)) state = continue state { props {mobileNumberEditFocused = focusChanged}} 
eval _ state = update state