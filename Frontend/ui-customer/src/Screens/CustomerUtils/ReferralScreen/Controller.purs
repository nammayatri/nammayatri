{-
 
  Copyright 2022-23, Juspay India Pvt Ltd
 
  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 
  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 
  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 
  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 
  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module Screens.ReferralScreen.Controller where

import Components.GenericHeader as GenericHeader
import Components.PrimaryButton as PrimaryButton
import Components.PrimaryEditText as PrimaryEditText
import Data.String as DS
import JBridge (hideKeyboardOnNavigation)
import Log (trackAppActionClick, trackAppBackPress, trackAppEndScreen, trackAppScreenRender, trackAppTextInput, trackAppScreenEvent)
import Prelude (class Show, bind, discard, not, pure, void, ($), (==), unit)
import PrestoDOM (class Loggable, Eval, continue, continueWithCmd, exit)
import Screens (ScreenName(..), getScreen)
import Screens.Types (ReferralScreenState)

instance showAction :: Show Action where
  show _ = ""

instance loggableAction :: Loggable Action where
  performLog action appId = case action of
    AfterRender -> trackAppScreenRender appId "screen" (getScreen REFERRAL_SCREEN)
    BackPressed -> do
      trackAppBackPress appId (getScreen REFERRAL_SCREEN)
      trackAppEndScreen appId (getScreen REFERRAL_SCREEN)
    ContinueButtonAC act -> case act of
      PrimaryButton.OnClick -> trackAppActionClick appId (getScreen REFERRAL_SCREEN) "continue_primary_button" "continue"
      PrimaryButton.NoAction -> trackAppScreenEvent appId (getScreen REFERRAL_SCREEN) "continue_primary_button" "no_action"
    GoToHomeButtonAC act -> case act of
      PrimaryButton.OnClick -> trackAppActionClick appId (getScreen REFERRAL_SCREEN) "go_to_home_primary_button" "go_to_home"
      PrimaryButton.NoAction -> trackAppScreenEvent appId (getScreen REFERRAL_SCREEN) "go_to_home_primary_button" "no_action"
    ReferralEditText act -> case act of
      PrimaryEditText.TextChanged _ _ -> trackAppTextInput appId (getScreen REFERRAL_SCREEN) "referral_code_text_changed" "primary_edit_text"
      PrimaryEditText.FocusChanged _ -> trackAppTextInput appId (getScreen REFERRAL_SCREEN) "referral_code_text_focus_changed" "primary_edit_text"
    GenericHeaderAC act -> case act of
      GenericHeader.PrefixImgOnClick -> do
        trackAppActionClick appId (getScreen REFERRAL_SCREEN) "generic_header_action" "back_icon"
        trackAppEndScreen appId (getScreen REFERRAL_SCREEN)
      GenericHeader.SuffixImgOnClick -> trackAppActionClick appId (getScreen REFERRAL_SCREEN) "generic_header_action" "forward_icon"
    ExpandReference -> trackAppActionClick appId (getScreen REFERRAL_SCREEN) "in_screen" "referral_program_drop_down"

data Action
  = AfterRender
  | ContinueButtonAC PrimaryButton.Action
  | ReferralEditText PrimaryEditText.Action
  | GenericHeaderAC GenericHeader.Action
  | ExpandReference
  | BackPressed
  | GoToHomeButtonAC PrimaryButton.Action

data ScreenOutput
  = UpdateReferral ReferralScreenState
  | GoToHome

eval :: Action -> ReferralScreenState -> Eval Action ScreenOutput ReferralScreenState
eval (ReferralEditText (PrimaryEditText.TextChanged id value)) state = do
  let
    refCode = DS.trim value

    btnActive = (DS.length refCode) == 6

    newState = state { referralCode = refCode, isInvalidCode = false }
  if btnActive then
    void $ pure $ hideKeyboardOnNavigation true
  else
    pure unit
  continue newState { btnActive = btnActive }

eval (ContinueButtonAC PrimaryButton.OnClick) state = do
  _ <- pure $ hideKeyboardOnNavigation true
  exit $ UpdateReferral state

eval (GenericHeaderAC (GenericHeader.PrefixImgOnClick)) state = continueWithCmd state [ do pure BackPressed ]

eval (GoToHomeButtonAC PrimaryButton.OnClick) state = continueWithCmd state [ do pure BackPressed ]

eval ExpandReference state = continue state { isExpandReference = not state.isExpandReference }

eval BackPressed state = do
  if state.isExpandReference then
    continue state { isExpandReference = not state.isExpandReference }
  else
    exit GoToHome

eval _ state = continue state
