{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.EnterMobileNumberScreen.Controller where
import Prelude (class Show, not, pure, unit, (&&), (<=), (==), (||), discard, bind, show, void, ($), (>), (/=), when)
import PrestoDOM (Eval, update, continue, continueWithCmd, exit, updateAndExit)
import Screens.Types (EnterMobileNumberScreenState)
import Components.PrimaryEditText.Controller as PrimaryEditText
import Components.MobileNumberEditor as MobileNumberEditor
import Components.PrimaryButton.Controller as PrimaryButton
import PrestoDOM.Types.Core (class Loggable)
import Data.String (length, trim)
import Data.String.CodeUnits (charAt)
import Data.Maybe (Maybe(..))
import JBridge (requestKeyboardShow,hideKeyboardOnNavigation, toast, scrollToEnd, validateEmail)
import Engineering.Helpers.Commons (getNewIDWithTag, oAuthSignIn, callbackMapper, setText)
import Effect.Class (liftEffect)
import Log (trackAppActionClick, trackAppEndScreen, trackAppScreenRender, trackAppBackPress, trackAppTextInput, trackAppScreenEvent)
import Screens (ScreenName(..), getScreen)
import Effect.Unsafe (unsafePerformEffect)
import Engineering.Helpers.LogEvent (logEvent)
import ConfigProvider
import Common.Types.App (MobileNumberValidatorResp(..)) as MVR
import Data.Function.Uncurried
import Effect.Uncurried (runEffectFn2, mkEffectFn4)
import Engineering.Helpers.Utils (mobileNumberValidator, mobileNumberMaxLength)
import PrestoDOM.Core (getPushFn)
import Language.Strings (getString)
import Language.Types (STR(..))
import Services.API (OAuthProvider(..))
import Mobility.Prelude (strToMaybe)
import Debug
import Data.Array as DA

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

data ScreenOutput = GoBack | GoToNextScreen EnterMobileNumberScreenState | OAuthReq EnterMobileNumberScreenState
data Action = BackPressed 
            | PrimaryEditTextAction MobileNumberEditor.Action
            | PrimaryButtonActionController PrimaryButton.Action
            | PrimaryButtonEmailAC PrimaryButton.Action
            | PrimaryButtonLoginWithMobileAC PrimaryButton.Action
            | NoAction
            | CheckBoxClicked
            | CheckClickability
            | AfterRender
            | NonDisclosureAgreementAction
            | OAuthPB OAuthProvider PrimaryButton.Action
            | EmailPBAC PrimaryEditText.Action
            | OAuthResponse String String String String
            | KeyboardCallback String

eval :: Action -> EnterMobileNumberScreenState -> Eval Action ScreenOutput EnterMobileNumberScreenState
eval AfterRender state = continue state
eval BackPressed state = do
        pure $ hideKeyboardOnNavigation true
        exit GoBack
eval (OAuthPB provider (PrimaryButton.OnClick)) state = do
  continueWithCmd state{data{oauthProvider = Just provider}} [do
    push <- getPushFn Nothing "EnterMobileNumberScreen"
    let callback = callbackMapper $ mkEffectFn4 \status token name email -> push $ OAuthResponse status token name email
    _ <- runEffectFn2 oAuthSignIn (show provider) callback
    pure NoAction
  ]
eval (PrimaryButtonActionController (PrimaryButton.OnClick)) state = exit (GoToNextScreen state)
eval (PrimaryEditTextAction (MobileNumberEditor.TextChanged valId newVal)) state = do
  let config = getAppConfig appConfig
  _ <- if length newVal ==  mobileNumberMaxLength config.defaultCountryCodeConfig.countryShortCode then do
            pure $ hideKeyboardOnNavigation true
            else pure unit
  let validatorResp = mobileNumberValidator config.defaultCountryCodeConfig.countryCode config.defaultCountryCodeConfig.countryShortCode newVal
  if isValidMobileNumber validatorResp then do 
    let _ = unsafePerformEffect $ logEvent state.data.logField "ny_driver_mobnum_entry"
    pure unit
    else pure unit
  continue  state { props = state.props { btnActive = (length newVal == 10 && (isValidMobileNumber validatorResp))
                                        , isValid = isValidMobileNumber validatorResp }
                                        , data = state.data { mobileNumber = if validatorResp == MVR.MaxLengthExceeded then state.data.mobileNumber else newVal}}
{-
    status - SUCCESS | FAILED
    token -  idTokenString | ErrorCode
    name -  name | empty
    email - email | empty
-}
eval (OAuthResponse status token name email) state = 
  if status == "SUCCESS" 
    then exit $ OAuthReq $ state{ data{ token = Just token, name = Just name, email = Just email}}
    else do
      if DA.elem token ["1001", "-5", "12501"] then pure unit
        else void $ pure $ toast $ getString SOMETHING_WENT_WRONG
      update state

eval (KeyboardCallback event) state = case event of
  "onKeyboardOpen" ->
    continueWithCmd state
        [ do 
          when (not state.props.loginWithMobileBtnClicked) $ do
            void $ scrollToEnd (getNewIDWithTag "OAuthScrollView") true
          pure NoAction
        ]
  _ -> update state

eval (EmailPBAC (PrimaryEditText.TextChanged id val)) state = do
  continue state{data{email = strToMaybe (trim val)}, props{isValid = true, btnActive =  true}}



eval (PrimaryButtonEmailAC PrimaryButton.OnClick) state = do 
  case state.data.email of
    Nothing -> continue state{props{isValid = false}}
    Just val -> do
      let trimmed = trim val
          validEmail = validateEmail $ trimmed
          newState = state{props{isValid = validEmail}}
      pure $ setText (getNewIDWithTag "EmailIdPrimaryEditText") ""
      if validEmail 
        then updateAndExit newState $ GoToNextScreen newState
        else continue newState

eval (PrimaryButtonLoginWithMobileAC PrimaryButton.OnClick) state = do 
  pure $ setText (getNewIDWithTag "EnterMobileNumberEditText") ""
  pure $ hideKeyboardOnNavigation true
  continue state { data {mobileNumber = "", email = Nothing}, props { loginWithMobileBtnClicked = true, isValid = true}}

eval _ state = continue state

isValidMobileNumber :: MVR.MobileNumberValidatorResp -> Boolean 
isValidMobileNumber resp = (resp == MVR.ValidPrefix || resp == MVR.Valid)
