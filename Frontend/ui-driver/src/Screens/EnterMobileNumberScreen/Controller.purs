{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Screens.EnterMobileNumberScreen.Controller where
import Prelude (class Show, not, pure, unit, (&&), (<=), (==), (||), discard, bind, ($), (>))
import PrestoDOM (Eval, continue, continueWithCmd, exit)
import Screens.Types (EnterMobileNumberScreenState)
import Components.PrimaryEditText.Controller as PrimaryEditText
import Components.PrimaryButton.Controller as PrimaryButton
import PrestoDOM.Types.Core (class Loggable)
import Data.String (length)
import Data.String.CodeUnits (charAt)
import Data.Maybe (Maybe(..))
import Components.StepsHeaderModel.Controller as StepsHeaderModelController
import JBridge (requestKeyboardShow,hideKeyboardOnNavigation)
import Engineering.Helpers.Commons (getNewIDWithTag)
import Effect.Class (liftEffect)
import Log (trackAppActionClick, trackAppEndScreen, trackAppScreenRender, trackAppBackPress, trackAppTextInput, trackAppScreenEvent)
import Screens (ScreenName(..), getScreen)

instance showAction :: Show Action where
  show _ = ""

instance loggableAction :: Loggable Action where
  performLog action appId = case action of
    AfterRender -> trackAppScreenRender appId "screen" (getScreen ENTER_MOBILE_NUMBER_SCREEN)
    BackPressed -> do
      trackAppBackPress appId (getScreen ENTER_MOBILE_NUMBER_SCREEN)
      trackAppEndScreen appId (getScreen ENTER_MOBILE_NUMBER_SCREEN)
    PrimaryEditTextAction act -> case act of
      --PrimaryEditText.OnClick -> trackAppActionClick appId (getScreen ENTER_MOBILE_NUMBER_SCREEN) "primary_edit_text" "on_click"
      PrimaryEditText.TextChanged valId newVal -> trackAppTextInput appId (getScreen ENTER_MOBILE_NUMBER_SCREEN) "mobile_number_text_changed" "primary_edit_text"
      PrimaryEditText.FocusChanged id -> trackAppTextInput appId (getScreen ENTER_MOBILE_NUMBER_SCREEN) "mobile_number_text_focus_changed" "primary_edit_text"
      --PrimaryEditText.TextClicked -> trackAppActionClick appId (getScreen ENTER_MOBILE_NUMBER_SCREEN) "primary_edit_text" "text_field_click"
    PrimaryButtonActionController act -> case act of
      PrimaryButton.OnClick -> do
        trackAppActionClick appId (getScreen ENTER_MOBILE_NUMBER_SCREEN) "primary_button" "next_on_click"
        trackAppEndScreen appId (getScreen ENTER_MOBILE_NUMBER_SCREEN)
      PrimaryButton.NoAction -> trackAppActionClick appId (getScreen ENTER_MOBILE_NUMBER_SCREEN) "primary_button" "no_action"
    NonDisclosureAgreementAction -> trackAppActionClick appId (getScreen ENTER_MOBILE_NUMBER_SCREEN) "in_screen" "no_discloure_agreement"
    CheckBoxClicked -> trackAppScreenEvent appId (getScreen ENTER_MOBILE_NUMBER_SCREEN) "in_screen" "checkbox_clicked"
    CheckClickability -> trackAppScreenEvent appId (getScreen ENTER_MOBILE_NUMBER_SCREEN) "in_screen" "check_clickability"
    -- StepsHeaderModelController.OnArrowClick -> trackAppScreenEvent appId (getScreen ENTER_MOBILE_NUMBER_SCREEN) "in_screen" "no_action"
    NoAction -> trackAppScreenEvent appId (getScreen ENTER_MOBILE_NUMBER_SCREEN) "in_screen" "no_action"
    _ -> trackAppScreenEvent appId (getScreen ENTER_MOBILE_NUMBER_SCREEN) "in_screen" "no_action"

data ScreenOutput = GoBack | GoToNextScreen EnterMobileNumberScreenState
data Action = BackPressed 
            | PrimaryEditTextAction PrimaryEditText.Action
            | PrimaryButtonActionController PrimaryButton.Action
            | NoAction
            | CheckBoxClicked
            | CheckClickability
            | AfterRender
            | NonDisclosureAgreementAction
            | StepsHeaderModelAC StepsHeaderModelController.Action

eval :: Action -> EnterMobileNumberScreenState -> Eval Action ScreenOutput EnterMobileNumberScreenState
eval AfterRender state = continue state
eval BackPressed state = do 
  _ <- pure $ hideKeyboardOnNavigation true
  exit GoBack
--eval (PrimaryEditTextAction PrimaryEditText.OnClick) state = continue state
eval (StepsHeaderModelAC StepsHeaderModelController.OnArrowClick) state = continueWithCmd state [ do pure $ BackPressed]
eval (PrimaryButtonActionController (PrimaryButton.OnClick)) state = exit (GoToNextScreen state)
eval (PrimaryEditTextAction (PrimaryEditText.FocusChanged val)) state = continue state{props {mobileNumberEditFocused = true}}
eval (PrimaryEditTextAction (PrimaryEditText.TextChanged valId newVal)) state = do
  _ <- if length newVal == 10 then do
            pure $ hideKeyboardOnNavigation true 
            else pure unit    
  let isValidMobileNumber = case (charAt 0 newVal) of 
                                    Just a -> if a=='0' || a=='1' || a=='2' || a=='5' then false 
                                                else if a=='3' || a=='4' then
                                                    if newVal=="4000400040" || newVal=="3000300030" then true else false 
                                                        else true 
                                    Nothing -> true 
  continue state { props = state.props { btnActive = if (length newVal == 10 && isValidMobileNumber) then true else false
                                        , isValid = isValidMobileNumber}
                                        , data = state.data { mobileNumber = if length newVal <= 10 then newVal else state.data.mobileNumber}}
eval _ state = continue state